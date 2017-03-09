/*
 * SchemeSmobValue.c
 *
 * Scheme small objects (SMOBS) for ProtoAtoms.
 *
 * Copyright (c) 2008,2009,2016 Linas Vepstas <linas@linas.org>
 */

#include <cstddef>
#include <libguile.h>

#include <opencog/atoms/base/FloatValue.h>
#include <opencog/atoms/base/LinkValue.h>
#include <opencog/atoms/base/StringValue.h>

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

	scm_remember_upto_here_1(svalue_list);
	return protom_to_scm(pa);
}

/* ============================================================== */

SCM SchemeSmob::ss_set_value (SCM satom, SCM skey, SCM svalue)
{
	Handle atom(verify_handle(satom, "cog-set-value!", 1));
	Handle key(verify_handle(skey, "cog-set-value!", 2));
	return satom;
}

SCM SchemeSmob::ss_value (SCM satom, SCM skey)
{
	Handle atom(verify_handle(satom, "cog-value", 1));
	Handle key(verify_handle(skey, "cog-value", 2));

	return protom_to_scm(atom->getValue(key));
}

/* ===================== END OF FILE ============================ */
