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
 */

#include <cstddef>
#include <libguile.h>

#include <opencog/atoms/core/NumberNode.h>
#include <opencog/atoms/value/BoolValue.h>
#include <opencog/atoms/value/Float32Value.h>
#include <opencog/atoms/value/FloatValue.h>
#include <opencog/atoms/value/LinkValue.h>
#include <opencog/atoms/value/StringValue.h>
#include <opencog/atoms/value/VoidValue.h>
#include <opencog/atoms/value/RandomStream.h>
#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/atom_types/NameServer.h>
#include <opencog/guile/SchemeSmob.h>

#include <opencog/atoms/value/ValueFactory.h>

using namespace opencog;

/* ============================================================== */
/** Return the type of a value/atom */

SCM SchemeSmob::from_type (const ValuePtr& vp)
{
	Type t = vp->get_type();
	const std::string &tname = nameserver().getTypeName(t);
	SCM str = scm_from_utf8_string(tname.c_str());
	SCM sym = scm_string_to_symbol(str);

	return sym;
}

SCM SchemeSmob::ss_type (SCM svalue)
{
	ValuePtr vp(scm_to_protom(svalue));
	if (nullptr == vp)
		return SCM_BOOL_F;

	if (vp->is_atom())
	{
		Handle h(HandleCast(vp));

		// Clobber pointer to extracted (deleted) Atoms. These are
		// Atoms that have been removed from an AtomSpace; they have
		// not yet been dtored, because the std::shared_ptr<> still
		// has a non-zero use-count. Fix that now, by calling
		// std::shared_ptr<>::reset() to clobber it. This is used by
		// various unit tests (including StorageNode) to verify that
		// an Atom has been extracted from an AtomSpace.
		//
		// We don't do this for Values; those will just live on,
		// until the guile gc runs and finds them; the clobber happens
		// in SchemeSmob::free_misc()
		if (nullptr == h->getAtomSpace() and
		    not (ATOM_SPACE == h->get_type()))
		{
			SCM_SMOB_VALUE_PTR_LOC(svalue)->reset(); // std::shared_ptr<>::reset()
			scm_remember_upto_here_1(svalue);
			return SCM_BOOL_F;
		}
	}
	scm_remember_upto_here_1(svalue);

	return from_type(vp);
}

/* ============================================================== */
/**
 * Convert argument into a list of bools.
 */
std::vector<bool>
SchemeSmob::verify_bool_list (SCM svalue_list, const char * subrname, int pos)
{
	// Verify that second arg is an actual list. Allow null list
	// (which is rather unusual, but legit.  Allow embedded nulls
	// as this can be convenient for writing scheme code.
	if (!scm_is_pair(svalue_list) and !scm_is_null(svalue_list))
		scm_wrong_type_arg_msg(subrname, pos, svalue_list, "a list of boolean values");
	return scm_to_bool_list(svalue_list);
}

std::vector<bool>
SchemeSmob::scm_to_bool_list (SCM svalue_list)
{
	SCM sl = svalue_list;

	// Flatten, if its a list...
	if (scm_is_pair(sl) and scm_is_pair(SCM_CAR(sl)))
		sl = SCM_CAR(sl);

	std::vector<bool> valist;
	while (scm_is_pair(sl)) {
		SCM svalue = SCM_CAR(sl);

		if (not scm_is_null(svalue)) {
			bool v;
			if (scm_is_bool(svalue))
				v = scm_to_bool(svalue);
			else if (scm_is_integer(svalue))
				v = scm_to_int8(svalue);
			else
				scm_wrong_type_arg_msg("cog-new-value", 2, svalue_list,
					"a list of boolean values");
			valist.emplace_back(v);
		}
		sl = SCM_CDR(sl);
	}
	return valist;
}


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
	SCM sl = svalue_list;

	// Flatten, if its a list...
	if (scm_is_pair(sl) and scm_is_pair(SCM_CAR(sl)))
		sl = SCM_CAR(sl);

	std::vector<ValuePtr> valist;
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
	SCM sl = svalue_list;

	// Flatten, if its a list...
	if (scm_is_pair(sl) and scm_is_pair(SCM_CAR(sl)))
		sl = SCM_CAR(sl);

	std::vector<std::string> valist;
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
 * Construct a new value, of type t, and (type constructor) arguments
 * given in svalue_list.
 *
 * There are two ways this could be implemented:
 * Option A)
 *    Create a factory for Values. Since we've got factories
 *    implemented for the C++ types, this seems plausible. However,
 *    actually writing code to do this, and making it run as a plug
 *    in module for other scheme modules not in the git repo... ouch.
 *    Seems complicated, and I could not think of a good way of doing
 *    this. So punt on the factory idea.
 *
 * Option B)
 *    Dispatch based on argument type. The type definition file
 *    indicates what kind of arguments the type constructors accept,
 *    and so we look at those, and if they match up with what the
 *    user passed in, then construct with those args. This is easy,
 *    its what is implemented below, and it works.
 */
ValuePtr SchemeSmob::make_value (Type t, SCM svalue_list)
{
	SCM first_arg = svalue_list;
	if (not scm_is_null(first_arg) and scm_is_pair(first_arg))
		first_arg = SCM_CAR(first_arg);

	// If first argument is again a list, flatten it. This arises
	// as a side effect when people write cmplicated scheme algos.
	if (not scm_is_null(first_arg) and scm_is_pair(first_arg))
		first_arg = SCM_CAR(first_arg);

	bool zero_args = scm_is_null(first_arg);

	if (nameserver().isA(t, VOID_ARG) and zero_args)
		return valueserver().create(t);

	// Is there only one argument grand total?
	bool just_one_arg =
		(not zero_args) and
		scm_is_pair(svalue_list) and
		scm_is_null(SCM_CDR(svalue_list));

	// First, look to see if explicit argument types are given.
	// If they are, and the scheme value matches the argument
	// type, then run the constructor for that argument type.
	if (just_one_arg)
	{
		if (nameserver().isA(t, STRING_ARG) and
		    scm_is_string(first_arg))
		{
			std::string name = verify_string(first_arg, "cog-new-value", 2);
			return valueserver().create(t, std::move(name));
		}

	   if (nameserver().isA(t, HANDLE_ARG))
		{
			ValuePtr vp(scm_to_protom(first_arg));
			if (vp and vp->is_atom())
			{
				Handle h(verify_handle(first_arg, "cog-new-value", 2));
				return valueserver().create(t, h);
			}
		}

		if (nameserver().isA(t, INT_ARG) and
		    scm_is_integer(first_arg))
		{
			int dim = verify_int(first_arg, "cog-new-value", 2);
			return valueserver().create(t, dim);
		}

	   if (nameserver().isA(t, VALUE_ARG))
		{
			ValuePtr vp(scm_to_protom(first_arg));
			return valueserver().create(t, vp);
		}

		// Hmm. We have to fall through, here, to handle the other cases.
		// scm_wrong_type_arg_msg("cog-new-value", 1, svalue_list, "single value");
	}

	// -------------------------
	// Everything below expects one or more args.
	// ... sort of. Anything that takes a vector argument also
	// allows a zero-length vector. But ins scheme, we don't know
	// the type of a zero-length vector ... so ... cross our fingers!?

	if (nameserver().isA(t, STRING_VEC_ARG) and
	    (scm_is_string(first_arg) or zero_args))
	{
		std::vector<std::string> valist;
		valist = verify_string_list(svalue_list, "cog-new-value", 2);
		return valueserver().create(t, valist);
	}

	if (nameserver().isA(t, FLOAT_VEC_ARG) and
	    (scm_is_number(first_arg) or zero_args))
	{
		std::vector<double> valist;
		valist = verify_float_list(svalue_list, "cog-new-value", 2);
		return valueserver().create(t, valist);
	}

	if (nameserver().isA(t, VALUE_VEC_ARG) and
	    (scm_is_protom(first_arg) or zero_args))
	{
		std::vector<ValuePtr> valist;
		valist = verify_protom_list(svalue_list, "cog-new-value", 2);
		return valueserver().create(t, valist);
	}

	if (nameserver().isA(t, HANDLE_VEC_ARG))
	{
		ValuePtr vp(scm_to_protom(first_arg));
		if (vp and vp->is_atom())
		{
			HandleSeq oset(verify_handle_list(svalue_list, "cog-new-value", 2));
			return valueserver().create(t, std::move(oset));
		}
		if (not vp and zero_args)
			return valueserver().create(t, std::move(HandleSeq()));
	}

	if (nameserver().isA(t, BOOL_VEC_ARG) and
	    (scm_is_bool(first_arg) or scm_is_integer(first_arg) or zero_args))
	{
		std::vector<bool> valist;
		valist = verify_bool_list(svalue_list, "cog-new-value", 2);
		return valueserver().create(t, valist);
	}

	if (nameserver().isA(t, NODE))
	{
		// XXX FIXME... at this time, nodes have a single name.
		SCM sname = SCM_CAR(svalue_list);
		std::string name = verify_string(sname, "cog-new-value", 2);
		const AtomSpacePtr& asp = ss_get_env_as("cog-new-value");
		return asp->add_node(t, std::move(name));
	}

	if (nameserver().isA(t, LINK))
	{
		HandleSeq oset = verify_handle_list(svalue_list, "cog-new-value", 2);
		const AtomSpacePtr& asp = ss_get_env_as("cog-new-value");
		return asp->add_link(t, std::move(oset));
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

SCM SchemeSmob::set_value (const Handle& atom, const Handle& key,
                           const ValuePtr& value, SCM satom,
                           const char* msg)
{
	// Note that pa might be a null pointer, if svalue is '() or #f
	// In this case, the key is removed.
	const AtomSpacePtr& asp = ss_get_env_as(msg);
	try
	{
		Handle newh = asp->set_value(atom, key, value);
		if (atom == newh) return satom;
		return handle_to_scm(newh);
	}
	catch (const std::exception& ex)
	{
		throw_exception(ex, msg, satom);
	}
}

SCM SchemeSmob::ss_set_value (SCM satom, SCM skey, SCM svalue)
{
	Handle atom(verify_handle(satom, "cog-set-value!", 1));
	Handle key(verify_handle(skey, "cog-set-value!", 2));

	// If no args, use VoidValue
	if (scm_is_null(svalue))
	{
		ValuePtr pa(createVoidValue());
		return set_value(atom, key, pa, satom, "cog-set-value!");
	}

	// If it is a list, assume its a list of values.
	if (not scm_is_null(SCM_CDR(svalue)))
	{
		std::vector<ValuePtr> vals = scm_to_protom_list(svalue);
		ValuePtr pa(createLinkValue(std::move(vals)));
		return set_value(atom, key, pa, satom, "cog-set-value!");
	}

	svalue = SCM_CAR(svalue);

	// If svalue is actually a value, just use it.
	if (not scm_is_pair(svalue))
	{
		ValuePtr pa(verify_protom(svalue, "cog-set-value!", 3));
		return set_value(atom, key, pa, satom, "cog-set-value!");
	}

	// Single argument that is not a Value. Convert.
	SCM sitem = SCM_CAR(svalue);

	if (scm_is_number(sitem))
	{
		ValuePtr pa(createFloatValue(scm_to_float_list(svalue)));
		return set_value(atom, key, pa, satom, "cog-set-value!");
	}

	if (scm_is_string(sitem))
	{
		ValuePtr pa(createStringValue(scm_to_string_list(svalue)));
		return set_value(atom, key, pa, satom, "cog-set-value!");
	}

	if (scm_is_symbol(sitem))
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
		return ss_set_value(satom, skey, scm_cons(newl, SCM_EOL));
	}

	// Hmm. Could be a list in a list. Tested in BasicSCMUTest
	if (scm_is_true(scm_list_p(svalue)))
	{
		verify_protom(sitem, "cog-set-value!", 3);
		ValuePtr pa(createLinkValue(scm_to_protom_list(svalue)));
		return set_value(atom, key, pa, satom, "cog-set-value!");
	}

	scm_wrong_type_arg_msg("cog-set-value!", 3, svalue,
		"zero or more Values");
}

// alist is an association-list of key-value pairs.
Handle SchemeSmob::set_values(const Handle& h, const AtomSpacePtr& asp, SCM alist)
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
			if (key) atom = asp->set_value(atom, key, pa);
		}
		kvpli = SCM_CDR(kvpli);
	}

	return atom;
}

SCM SchemeSmob::ss_value (SCM satom, SCM skey)
{
	Handle atom(verify_handle(satom, "cog-value", 1));
	Handle key(verify_handle(skey, "cog-value", 2));

	return protom_to_scm(atom->getValue(key));
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

	if (nameserver().isA(t, FLOAT32_VALUE))
	{
		const std::vector<float>& v = Float32ValueCast(pa)->value();
		CPPL_TO_SCML(v, scm_from_double)
	}

	if (nameserver().isA(t, FLOAT_VALUE))
	{
		const std::vector<double>& v = FloatValueCast(pa)->value();
		CPPL_TO_SCML(v, scm_from_double)
	}

	if (nameserver().isA(t, BOOL_VALUE))
	{
		const std::vector<bool>& v = BoolValueCast(pa)->value();
		CPPL_TO_SCML(v, scm_from_bool)
	}

	if (nameserver().isA(t, STRING_VALUE))
	{
		const std::vector<std::string>& v = StringValueCast(pa)->value();
		CPPL_TO_SCML(v, scm_from_string)
	}

	if (nameserver().isA(t, LINK_VALUE))
	{
		const std::vector<ValuePtr>& v = LinkValueCast(pa)->value();
		CPPL_TO_SCML(v, protom_to_scm)
	}

	if (nameserver().isA(t, LINK) or
	    nameserver().isA(t, FRAME))
	{
		const HandleSeq& v = HandleCast(pa)->getOutgoingSet();
		CPPL_TO_SCML(v, handle_to_scm)
	}

	if (nameserver().isA(t, NODE))
	{
		if (nameserver().isA(t, NUMBER_NODE))
		{
			const std::vector<double>& v = NumberNodeCast(pa)->value();
			CPPL_TO_SCML(v, scm_from_double)
		}
		const std::string& name = HandleCast(pa)->get_name();
		return scm_cons(scm_from_utf8_string(name.c_str()), SCM_EOL);
	}

	return SCM_EOL;
}

// This has two forms:
// If two arguments, then VALUE and INDEX
// If three arguments, then ATOM KEY INDEX
// Decode which.
SCM SchemeSmob::ss_value_ref (SCM s1, SCM s2, SCM s3)
{
	// There isn't any third argument, assume s1 is a Value.
	if (scm_is_false(scm_integer_p(s3)))
	{
		ValuePtr pa(verify_protom(s1, "cog-value-ref", 1));
		size_t index = verify_size_t(s2, "cog-value-ref", 2);
		return value_ref(pa, index);
	}

	// If there are three args, the first two are Atom and Key
	size_t index = (size_t) scm_to_long(s3);
	Handle atom(verify_handle(s1, "cog-value-ref", 1));
	Handle key(verify_handle(s2, "cog-value-ref", 2));

	ValuePtr pa(atom->getValue(key));
	if (pa)
		return value_ref(pa, index);

	SCM ilist = scm_cons(s1, SCM_EOL);
	ilist = scm_cons(s2, ilist);
	scm_error(
		scm_from_utf8_symbol("out-of-range"),
		"cog-value-ref",
		"No Value at key ~A on Atom ~A",
		ilist,
		SCM_BOOL_F);
}

SCM SchemeSmob::value_ref (const ValuePtr& pa, size_t index)
{
	Type t = pa->get_type();

	if (nameserver().isA(t, FLOAT32_VALUE))
	{
		const std::vector<float>& v = Float32ValueCast(pa)->value();
		if (index < v.size()) return scm_from_double(v[index]);
	}

	else if (nameserver().isA(t, FLOAT_VALUE))
	{
		const std::vector<double>& v = FloatValueCast(pa)->value();
		if (index < v.size()) return scm_from_double(v[index]);
	}

	else if (nameserver().isA(t, BOOL_VALUE))
	{
		const std::vector<bool>& v = BoolValueCast(pa)->value();
		if (index < v.size()) return scm_from_bool(v[index]);
	}

	else if (nameserver().isA(t, STRING_VALUE))
	{
		const std::vector<std::string>& v = StringValueCast(pa)->value();
		if (index < v.size()) return scm_from_string(v[index]);
	}

	else if (nameserver().isA(t, LINK_VALUE))
	{
		const std::vector<ValuePtr>& v = LinkValueCast(pa)->value();
		if (index < v.size()) return protom_to_scm(v[index]);
	}

	else if (nameserver().isA(t, LINK) or
	         nameserver().isA(t, FRAME))
	{
		const HandleSeq& v = HandleCast(pa)->getOutgoingSet();
		if (index < v.size()) return handle_to_scm(v[index]);
	}

	else if (nameserver().isA(t, NODE))
	{
		if (nameserver().isA(t, NUMBER_NODE))
		{
			const std::vector<double>& v = NumberNodeCast(pa)->value();
			if (index < v.size()) return scm_from_double(v[index]);
		}
		else
		{
			const std::string& name = HandleCast(pa)->get_name();
			if (0 == index) return scm_from_string(name);
		}
	}

	SCM ilist = scm_cons(protom_to_scm(pa), SCM_EOL);
	ilist = scm_cons(scm_from_int(index), ilist);
	scm_error(
		scm_from_utf8_symbol("out-of-range"),
		"cog-value-ref",
		"Index ~A is out of range for Value ~A",
		ilist,
		SCM_BOOL_F);

	// Hmm. scm_error never returns.
	return SCM_EOL;
}

/* ===================== END OF FILE ============================ */
