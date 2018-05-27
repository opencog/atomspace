/*
 * SchemeSmobValue.c
 *
 * Scheme small objects (SMOBS) for ProtoAtoms.
 *
 * Copyright (c) 2008,2009,2016 Linas Vepstas <linas@linas.org>
 */

#include <cstddef>
#include <libguile.h>

#include <opencog/atoms/proto/FloatValue.h>
#include <opencog/atoms/proto/LinkValue.h>
#include <opencog/atoms/proto/StringValue.h>
#include <opencog/atoms/proto/RandomStream.h>
#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/proto/NameServer.h>

#include <opencog/guile/SchemeSmob.h>

using namespace opencog;

/* ============================================================== */
/** Return true if s is a value */

SCM SchemeSmob::ss_value_p (SCM s)
{
	if (not SCM_SMOB_PREDICATE(SchemeSmob::cog_misc_tag, s))
		return SCM_BOOL_F;

	scm_t_bits misctype = SCM_SMOB_FLAGS(s);
	if (COG_PROTOM == misctype)
		return SCM_BOOL_T;

	return SCM_BOOL_F;
}

/* ============================================================== */
/**
 * Convert argument into a list of floats.
 */
std::vector<double>
SchemeSmob::verify_float_list (SCM svalue_list, const char * subrname, int pos)
{
	// Verify that second arg is an actual list. Allow null list
	// (which is rather unusual, but legit.  Allow embedded nulls
	// as this can be convenient for writing scheme code.
	if (!scm_is_pair(svalue_list) and !scm_is_null(svalue_list))
		scm_wrong_type_arg_msg(subrname, pos, svalue_list, "a list of float-pt values");
	return scm_to_float_list(svalue_list);
}

std::vector<double>
SchemeSmob::scm_to_float_list (SCM svalue_list)
{
	std::vector<double> valist;
	SCM sl = svalue_list;
	while (scm_is_pair(sl)) {
		SCM svalue = SCM_CAR(sl);

		if (not scm_is_null(svalue)) {
			double v = scm_to_double(svalue);
			valist.emplace_back(v);
		}
		sl = SCM_CDR(sl);
	}
	return valist;
}

/**
 * Convert argument into a list of protoatoms.
 */
std::vector<ProtoAtomPtr>
SchemeSmob::verify_protom_list (SCM svalue_list, const char * subrname, int pos)
{
	// Verify that second arg is an actual list. Allow null list
	// (which is rather unusual, but legit.  Allow embedded nulls
	// as this can be convenient for writing scheme code.
	if (!scm_is_pair(svalue_list) and !scm_is_null(svalue_list))
		scm_wrong_type_arg_msg(subrname, pos, svalue_list, "a list of protoatom values");
	return scm_to_protom_list(svalue_list);
}

std::vector<ProtoAtomPtr>
SchemeSmob::scm_to_protom_list (SCM svalue_list)
{
	std::vector<ProtoAtomPtr> valist;
	SCM sl = svalue_list;
	while (scm_is_pair(sl)) {
		SCM svalue = SCM_CAR(sl);

		if (not scm_is_null(svalue)) {
			ProtoAtomPtr pa(scm_to_protom(svalue));
			valist.emplace_back(pa);
		}
		sl = SCM_CDR(sl);
	}
	return valist;
}

/**
 * Convert argument into a list of strings.
 */
std::vector<std::string>
SchemeSmob::verify_string_list (SCM svalue_list, const char * subrname, int pos)
{
	// Verify that second arg is an actual list. Allow null list
	// (which is rather unusual, but legit).  Allow embedded nulls,
	// as this can be convenient for writing scheme code.
	if (!scm_is_pair(svalue_list) and !scm_is_null(svalue_list))
		scm_wrong_type_arg_msg(subrname, pos, svalue_list, "a list of string values");

	return scm_to_string_list(svalue_list);
}

std::vector<std::string>
SchemeSmob::scm_to_string_list (SCM svalue_list)
{
	std::vector<std::string> valist;
	SCM sl = svalue_list;
	while (scm_is_pair(sl)) {
		SCM svalue = SCM_CAR(sl);

		if (not scm_is_null(svalue)) {
			char * v = scm_to_utf8_string(svalue);
			valist.emplace_back(v);
		}
		sl = SCM_CDR(sl);
	}
	return valist;
}

/* ============================================================== */
/**
 * Create a new value, of named type stype, and value vector svect
 * XXX FIXME Clearly, a factory for values is called for.
 */
SCM SchemeSmob::ss_new_value (SCM stype, SCM svalue_list)
{
	Type t = verify_atom_type(stype, "cog-new-value", 1);

	ProtoAtomPtr pa;
	if (FLOAT_VALUE == t)
	{
		std::vector<double> valist;
		valist = verify_float_list(svalue_list, "cog-new-value", 2);
		pa = createFloatValue(valist);
	}

	else if (RANDOM_STREAM == t)
	{
		if (!scm_is_pair(svalue_list) and !scm_is_null(svalue_list))
			scm_wrong_type_arg_msg("cog-new-value", 1,
				svalue_list, "an optional dimension");
		int dim = 1;

		if (!scm_is_null(svalue_list))
		{
			SCM svalue = SCM_CAR(svalue_list);
			dim = verify_int(svalue, "cog-new-value", 2);
		}
		pa = createRandomStream(dim);
	}

	else if (LINK_VALUE == t)
	{
		std::vector<ProtoAtomPtr> valist;
		valist = verify_protom_list(svalue_list, "cog-new-value", 2);
		pa = createLinkValue(valist);
	}

	else if (STRING_VALUE == t)
	{
		std::vector<std::string> valist;
		valist = verify_string_list(svalue_list, "cog-new-value", 2);
		pa = createStringValue(valist);
	}

	else
	{
		scm_wrong_type_arg_msg("cog-new-value", 1, svalue_list, "value type");
	}

	scm_remember_upto_here_1(svalue_list);
	return protom_to_scm(pa);
}

/* ============================================================== */

SCM SchemeSmob::ss_set_value (SCM satom, SCM skey, SCM svalue)
{
	Handle atom(verify_handle(satom, "cog-set-value!", 1));
	Handle key(verify_handle(skey, "cog-set-value!", 2));

	// If svalue is actually a value, just use it.
	// If it is a list, assume its a list of values.
	ProtoAtomPtr pa;
	if (scm_is_pair(svalue)) {
		SCM sitem = SCM_CAR(svalue);

		if (scm_is_number(sitem))
		{
			std::vector<double> fl = scm_to_float_list(svalue);
			pa = createFloatValue(fl);
		}
		else if (scm_is_string(sitem))
		{
			std::vector<std::string> fl = scm_to_string_list(svalue);
			pa = createStringValue(fl);
		}
		else if (scm_is_symbol(sitem))
		{
			// The code below allows the following to be evaluated:
			// (define x 0.44) (define y 0.55)
			// (cog-set-value! (Concept "foo") (Predicate "bar") '(x y))
			// Here, x and y are symbols, the symbol lookup gives
			// variables, and the variable deref gives 0.44, 0.55.
			SCM sl = svalue;
			SCM newl = SCM_EOL;
			while (scm_is_pair(sl)) {
				SCM sym = SCM_CAR(sl);
				if (scm_is_symbol(sym))
					newl = scm_cons(scm_variable_ref(scm_lookup(sym)), newl);
				else if (scm_is_true(scm_variable_p(sym)))
					newl = scm_cons(scm_variable_ref(sym), newl);
				else
					newl = scm_cons(sym, newl);
				sl = SCM_CDR(sl);
			}
			newl = scm_reverse(newl);
			return ss_set_value(satom, skey, newl);
		}
		else if (scm_is_true(scm_list_p(svalue)))
		{
			verify_protom(sitem, "cog-set-value!", 3);
			std::vector<ProtoAtomPtr> fl = scm_to_protom_list(svalue);
			pa = createLinkValue(fl);
		}
		else
		{
			scm_wrong_type_arg_msg("cog-set-value!", 3, svalue,
				"a list of protoatom values");
		}
	}
	// Strange! According to my reading of the guile source code,
	// scm_is_true() should return 0 if svalue is null, but strangely
	// it doesn't actually do that, so we need to explicitly test.
	else if (scm_is_true(svalue) and scm_is_false(scm_null_p(svalue)))
	{
		pa = verify_protom(svalue, "cog-set-value!", 3);
	}

	// Note that pa might be a null pointer, if svalue is '() or #f
	// In this case, the key is removed.
	atom->setValue(key, pa);
	return satom;
}

SCM SchemeSmob::ss_value (SCM satom, SCM skey)
{
	Handle atom(verify_handle(satom, "cog-value", 1));
	Handle key(verify_handle(skey, "cog-value", 2));

	try
	{
		return protom_to_scm(atom->getValue(key));
	}
	catch (const std::exception& ex)
	{
		throw_exception(ex, "cog-value", scm_cons(satom, skey));
	}
	return SCM_EOL;
}

/** Return all of the keys on the atom */
SCM SchemeSmob::ss_keys (SCM satom)
{
	Handle atom(verify_handle(satom, "cog-value"));
	AtomSpace* as = atom->getAtomSpace();

	SCM rv = SCM_EOL;
	HandleSet keys = atom->getKeys();
	for (const Handle& k : keys)
	{
		// OK, this is kind-of weird and hacky, but if the keys
		// are not in any atomspace at the time that we go to
		// print them, they'll be converted to <undefined handle>.
		// So we shove them into the same atomspace as the atom
		// itself. I don't quite like this, but it seems to be
		// needed to fit user expectations.
		if (as)
			rv = scm_cons (handle_to_scm(as->add_atom(k)), rv);
		else
			rv = scm_cons (handle_to_scm(k), rv);
	}
	return rv;
}

/* ============================================================== */
/** Return a scheme list of the values associated with the value */

#define CPPL_TO_SCML(VAL, FN) \
	SCM list = SCM_EOL; \
	for (int i = VAL.size()-1; i >= 0; i--) { \
		SCM smob = FN(VAL[i]); \
		list = scm_cons (smob, list); \
	} \
	return list;

static SCM scm_from_string(const std::string& str)
{
	return scm_from_utf8_string(str.c_str());
}

SCM SchemeSmob::ss_value_to_list (SCM svalue)
{
	ProtoAtomPtr pa(verify_protom(svalue, "cog-value->list"));
	Type t = pa->get_type();

	if (nameserver().isA(t, FLOAT_VALUE))
	{
		const std::vector<double>& v = FloatValueCast(pa)->value();
		CPPL_TO_SCML(v, scm_from_double)
	}

	if (STRING_VALUE == t)
	{
		const std::vector<std::string>& v = StringValueCast(pa)->value();
		CPPL_TO_SCML(v, scm_from_string)
	}

	if (LINK_VALUE == t)
	{
		const std::vector<ProtoAtomPtr>& v = LinkValueCast(pa)->value();
		CPPL_TO_SCML(v, protom_to_scm)
	}

	if (nameserver().isA(t, LINK))
	{
		const HandleSeq& v = AtomCast(pa)->getOutgoingSet();
		CPPL_TO_SCML(v, handle_to_scm)
	}

	if (nameserver().isA(t, NODE))
	{
		const std::string& name = AtomCast(pa)->get_name();
		return scm_cons(scm_from_utf8_string(name.c_str()), SCM_EOL);
	}

	return SCM_EOL;
}

SCM SchemeSmob::ss_value_ref (SCM svalue, SCM sindex)
{
	ProtoAtomPtr pa(verify_protom(svalue, "cog-value-ref"));
   size_t index = verify_size(sindex, "cog-value-ref", 2);
	Type t = pa->get_type();

	if (nameserver().isA(t, FLOAT_VALUE))
	{
		const std::vector<double>& v = FloatValueCast(pa)->value();
		if (index < v.size()) return scm_from_double(v[index]);
	}

	if (STRING_VALUE == t)
	{
		const std::vector<std::string>& v = StringValueCast(pa)->value();
		if (index < v.size()) return scm_from_string(v[index]);
	}

	if (LINK_VALUE == t)
	{
		const std::vector<ProtoAtomPtr>& v = LinkValueCast(pa)->value();
		if (index < v.size()) return protom_to_scm(v[index]);
	}

	if (nameserver().isA(t, LINK))
	{
		const HandleSeq& v = AtomCast(pa)->getOutgoingSet();
		if (index < v.size()) return handle_to_scm(v[index]);
	}

	if (nameserver().isA(t, NODE))
	{
		const std::string& name = AtomCast(pa)->get_name();
		if (0 == index) return scm_from_string(name);
	}

	SCM ilist = scm_cons(scm_from_int(index), SCM_EOL);
	scm_error_scm(
		scm_from_utf8_symbol("out-of-range"),
		scm_from_utf8_string("cog-value-ref"),
		scm_from_utf8_string("Index of ~A is out of range"),
		ilist,
		ilist);

	// Hmm. scm_error never returns.
	return SCM_EOL;
}

/* ===================== END OF FILE ============================ */
