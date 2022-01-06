/* 
 * SchemeSmobValue.cc
 *
 * Scheme small objects (SMOBS) for ProtoAtoms [now renamed Value].
 *
 * Copyright (c) 2008,2009,2016 Linas Vepstas <linas@linas.org>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <cstddef>
#include <libguile.h>

#include <opencog/atoms/core/NumberNode.h>
#include <opencog/atoms/value/FloatValue.h>
#include <opencog/atoms/value/LinkValue.h>
#include <opencog/atoms/value/StringValue.h>
#include <opencog/atoms/value/RandomStream.h>
#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/atom_types/NameServer.h>
#include <opencog/guile/SchemeSmob.h>

#include <opencog/atoms/value/ValueFactory.h>

using namespace opencog;

/* ============================================================== */
/** Return the type of a value/atom */

SCM SchemeSmob::ss_type (SCM svalue)
{
	ValuePtr pa(verify_protom(svalue, "cog-type"));
	Type t = pa->get_type();
	const std::string &tname = nameserver().getTypeName(t);
	SCM str = scm_from_utf8_string(tname.c_str());
	SCM sym = scm_string_to_symbol(str);

	return sym;
}

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
	SCM sl = svalue_list;

	// Flatten, if its a list...
	if (scm_is_pair(sl) and scm_is_pair(SCM_CAR(sl)))
		sl = SCM_CAR(sl);

	std::vector<double> valist;
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
std::vector<ValuePtr>
SchemeSmob::verify_protom_list (SCM svalue_list, const char * subrname, int pos)
{
	// Verify that second arg is an actual list. Allow null list
	// (which is rather unusual, but legit.  Allow embedded nulls
	// as this can be convenient for writing scheme code.
	if (!scm_is_pair(svalue_list) and !scm_is_null(svalue_list))
		scm_wrong_type_arg_msg(subrname, pos, svalue_list, "a list of protoatom values");
	return scm_to_protom_list(svalue_list);
}

std::vector<ValuePtr>
SchemeSmob::scm_to_protom_list (SCM svalue_list)
{
	std::vector<ValuePtr> valist;
	SCM sl = svalue_list;
	while (scm_is_pair(sl)) {
		SCM svalue = SCM_CAR(sl);

		if (not scm_is_null(svalue)) {

			// Handle recursively nested lists
			if (scm_is_pair(svalue)) {
				std::vector<ValuePtr> vpl = scm_to_protom_list(svalue);
				valist.reserve(valist.size() + vpl.size());
				std::move(vpl.begin(), vpl.end(), std::back_inserter(valist));
			}
			else
			{
				ValuePtr pa(scm_to_protom(svalue));
				if (pa) valist.emplace_back(pa);
			}
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
ValuePtr SchemeSmob::make_value (Type t, SCM svalue_list)
{
	if (RANDOM_STREAM == t)
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
		return valueserver().create(t, dim);
	}

	if (nameserver().isA(t, FORMULA_STREAM))
	{
		if (!scm_is_pair(svalue_list))
			scm_wrong_type_arg_msg("cog-new-value", 1,
				svalue_list, "An Atom");

		SCM svalue = SCM_CAR(svalue_list);
		Handle h = verify_handle(svalue, "cog-new-value", 2);
		return valueserver().create(t, h);
	}

	// Catch and handle generic FloatValues not named above.
	if (nameserver().isA(t, FLOAT_VALUE))
	{
		std::vector<double> valist;
		valist = verify_float_list(svalue_list, "cog-new-value", 2);
		return valueserver().create(t, valist);
	}

	if (nameserver().isA(t, LINK_VALUE))
	{
		std::vector<ValuePtr> valist;
		valist = verify_protom_list(svalue_list, "cog-new-value", 2);
		return valueserver().create(t, valist);
	}

	if (nameserver().isA(t, STRING_VALUE))
	{
		std::vector<std::string> valist;
		valist = verify_string_list(svalue_list, "cog-new-value", 2);
		return valueserver().create(t, valist);
	}

	if (nameserver().isA(t, VOID_VALUE))
	{
		return valueserver().create(t);
	}

	if (nameserver().isA(t, NODE))
	{
		// XXX FIXME... at this time, nodes have a single name.
		SCM sname = SCM_CAR(svalue_list);
		std::string name = verify_string(sname, "cog-new-value", 2);
		AtomSpace* atomspace = ss_get_env_as("cog-new-value");
		return atomspace->add_node(t, std::move(name));
	}

	if (nameserver().isA(t, LINK))
	{
		HandleSeq oset = verify_handle_list(svalue_list, "cog-new-value", 2);
		AtomSpace* atomspace = ss_get_env_as("cog-new-value");
		return atomspace->add_link(t, std::move(oset));
	}

	scm_wrong_type_arg_msg("cog-new-value", 1, svalue_list, "value type");

	return nullptr;
}

SCM SchemeSmob::ss_new_value (SCM stype, SCM svalue_list)
{
	Type t = verify_type(stype, "cog-new-value", 1);
	try
	{
		ValuePtr pa = make_value(t, svalue_list);
		return protom_to_scm(pa);
	}
	catch (const std::exception& ex)
	{
		throw_exception(ex, "cog-new-value!", scm_cons(stype, svalue_list));
	}
	return SCM_EOL;
}

/* ============================================================== */

SCM SchemeSmob::ss_set_value (SCM satom, SCM skey, SCM svalue)
{
	Handle atom(verify_handle(satom, "cog-set-value!", 1));
	Handle key(verify_handle(skey, "cog-set-value!", 2));

	// If svalue is actually a value, just use it.
	// If it is a list, assume its a list of values.
	ValuePtr pa;
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
			std::vector<ValuePtr> fl = scm_to_protom_list(svalue);
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
	AtomSpace* as = ss_get_env_as("cog-set-value!");
	try
	{
		Handle newh = as->set_value(atom, key, pa);
		if (atom == newh) return satom;
		return handle_to_scm(newh);
	}
	catch (const std::exception& ex)
	{
		throw_exception(ex, "cog-set-value!", satom);
	}
}

// alist is an association-list of key-value pairs.
Handle SchemeSmob::set_values(const Handle& h, AtomSpace* as, SCM alist)
{
	Handle atom = h;
	SCM kvpli = alist;
	while (scm_is_pair(kvpli))
	{
		SCM kvp = SCM_CAR(kvpli);

		if (scm_is_pair(kvp))
		{
			Handle key(scm_to_handle(SCM_CAR(kvp)));
			ValuePtr pa(scm_to_protom(SCM_CDR(kvp)));
			if (key) atom = as->set_value(atom, key, pa);
		}
		kvpli = SCM_CDR(kvpli);
	}

	return atom;
}

// alist is an association-list of key-value pairs.
SCM SchemeSmob::ss_set_values(SCM satom, SCM alist)
{
	AtomSpace* as = ss_get_env_as("cog-set-values!");

	Handle atom(verify_handle(satom, "cog-set-values!", 1));
	if (not scm_is_true(scm_list_p(alist)))
		scm_wrong_type_arg_msg("cog-set-values!", 2, alist, "list of key-value pairs");

	Handle oldh = atom;

	// Atomspace may be read-only. Respect that.
	try
	{
		atom = set_values(atom, as, alist);
	}
	catch (const std::exception& ex)
	{
		throw_exception(ex, "cog-set-values!", satom);
	}

	// Atomspace may have given us a new atom...
	if (oldh == atom) return satom;

	return handle_to_scm(atom);
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
	Handle atom(verify_handle(satom, "cog-keys"));
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

/** Return association list of keys+values on the atom */
SCM SchemeSmob::ss_keys_alist (SCM satom)
{
	Handle atom(verify_handle(satom, "cog-keys->alist"));
	AtomSpace* as = atom->getAtomSpace();

	SCM rv = SCM_EOL;
	HandleSet keys = atom->getKeys();
	for (const Handle& k : keys)
	{
		ValuePtr vp = atom->getValue(k);

		// OK, this is kind-of weird and hacky, but if the keys
		// are not in any atomspace at the time that we go to
		// print them, they'll be converted to <undefined handle>.
		// So we shove them into the same atomspace as the atom
		// itself. I don't quite like this, but it seems to be
		// needed to fit user expectations.
		SCM pair;
		if (as)
			pair = scm_cons (handle_to_scm(as->add_atom(k)), protom_to_scm(vp));
		else
			pair = scm_cons (handle_to_scm(k), protom_to_scm(vp));

		rv = scm_cons (pair, rv);
	}
	return rv;
}

/* ============================================================== */
/** Return a scheme list of the values associated with the value */

#define CPPL_TO_SCML(VAL, FN) \
	SCM list = SCM_EOL; \
	for (size_t i = VAL.size(); i > 0; i--) { \
		SCM smob = FN(VAL[i-1]); \
		list = scm_cons (smob, list); \
	} \
	return list;

static SCM scm_from_string(const std::string& str)
{
	return scm_from_utf8_string(str.c_str());
}

SCM SchemeSmob::ss_value_to_list (SCM svalue)
{
	ValuePtr pa(verify_protom(svalue, "cog-value->list"));
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

	if (nameserver().isA(t, LINK_VALUE))
	{
		const std::vector<ValuePtr>& v = LinkValueCast(pa)->value();
		CPPL_TO_SCML(v, protom_to_scm)
	}

	if (nameserver().isA(t, LINK))
	{
		const HandleSeq& v = AtomCast(pa)->getOutgoingSet();
		CPPL_TO_SCML(v, handle_to_scm)
	}

	if (nameserver().isA(t, NODE))
	{
		if (nameserver().isA(t, NUMBER_NODE))
		{
			const std::vector<double>& v = NumberNodeCast(pa)->value();
			CPPL_TO_SCML(v, scm_from_double)
		}
		const std::string& name = AtomCast(pa)->get_name();
		return scm_cons(scm_from_utf8_string(name.c_str()), SCM_EOL);
	}

	return SCM_EOL;
}

SCM SchemeSmob::ss_value_ref (SCM svalue, SCM sindex)
{
	ValuePtr pa(verify_protom(svalue, "cog-value-ref"));
	size_t index = verify_size_t(sindex, "cog-value-ref", 2);
	Type t = pa->get_type();

	if (nameserver().isA(t, FLOAT_VALUE))
	{
		const std::vector<double>& v = FloatValueCast(pa)->value();
		if (index < v.size()) return scm_from_double(v[index]);
	}

	if (nameserver().isA(t, STRING_VALUE))
	{
		const std::vector<std::string>& v = StringValueCast(pa)->value();
		if (index < v.size()) return scm_from_string(v[index]);
	}

	if (nameserver().isA(t, LINK_VALUE))
	{
		const std::vector<ValuePtr>& v = LinkValueCast(pa)->value();
		if (index < v.size()) return protom_to_scm(v[index]);
	}

	if (nameserver().isA(t, LINK))
	{
		const HandleSeq& v = AtomCast(pa)->getOutgoingSet();
		if (index < v.size()) return handle_to_scm(v[index]);
	}

	if (nameserver().isA(t, NODE))
	{
		if (nameserver().isA(t, NUMBER_NODE))
		{
			const std::vector<double>& v = NumberNodeCast(pa)->value();
			if (index < v.size()) return scm_from_double(v[index]);
		}
		else
		{
			const std::string& name = AtomCast(pa)->get_name();
			if (0 == index) return scm_from_string(name);
		}
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
