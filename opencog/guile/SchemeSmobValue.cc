/*
 * SchemeSmobValue.c
 *
 * Scheme small objects (SMOBS) for ProtoAtoms.
 *
 * Copyright (c) 2008,2009,2016 Linas Vepstas <linas@linas.org>
 */

#ifdef HAVE_GUILE

#include <cstddef>
#include <libguile.h>

#include <opencog/truthvalue/AttentionValue.h>
#include <opencog/guile/SchemeSmob.h>

using namespace opencog;

/* ============================================================== */
#if 0
/**
 * Search for an attention value in a list of values.
 * Return the attention value if found, else return null.
 * Throw errors if the list is not stictly just key-value pairs
 */
AttentionValue * SchemeSmob::get_av_from_list(SCM slist)
{
	while (scm_is_pair(slist))
	{
		SCM sval = SCM_CAR(slist);
		if (SCM_SMOB_PREDICATE(SchemeSmob::cog_misc_tag, sval))
		{
			scm_t_bits misctype = SCM_SMOB_FLAGS(sval);
			switch (misctype)
			{
				case COG_Value:
					return (AttentionValue *) SCM_SMOB_DATA(sval);
				default:
					break;
			}
		}

		slist = SCM_CDR(slist);
	}

	return NULL;
}

/* ============================================================== */
/**
 * Take over memory management of an attention value
 */
SCM SchemeSmob::take_av (AttentionValue *av)
{
	scm_gc_register_collectable_memory (av,
	                 sizeof(*av), "opencog av");

	SCM smob;
	SCM_NEWSMOB (smob, cog_misc_tag, av);
	SCM_SET_SMOB_FLAGS(smob, COG_Value);
	return smob;
}

#endif

/* ============================================================== */
/** Return true if s is a value */

SCM SchemeSmob::ss_value_p (SCM s)
{
	if (not SCM_SMOB_PREDICATE(SchemeSmob::cog_misc_tag, s))
		return SCM_BOOL_F;

	scm_t_bits misctype = SCM_SMOB_FLAGS(s);
	if (COG_HANDLE == misctype)
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
		scm_wrong_type_arg_msg(subrname, pos, svalue_list, "a list of (float-pt) values");

	std::vector<double> valist;
	SCM sl = svalue_list;
	pos = 2;
	while (scm_is_pair(sl)) {
		SCM svalue = SCM_CAR(sl);

		if (not scm_is_null(svalue)) {
			double v = scm_to_double(svalue);
			valist.emplace_back(v);
		}
		sl = SCM_CDR(sl);
		pos++;
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
		scm_wrong_type_arg_msg(subrname, pos, svalue_list, "a list of (protoato) values");

	std::vector<ProtoAtomPtr> valist;
	SCM sl = svalue_list;
	pos = 2;
	while (scm_is_pair(sl)) {
		SCM svalue = SCM_CAR(sl);

		if (not scm_is_null(svalue)) {
			Handle h(scm_to_handle(svalue));
			ProtoAtomPtr pa(AtomCast(h));
			valist.emplace_back(pa);
		}
		sl = SCM_CDR(sl);
		pos++;
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
	// (which is rather unusual, but legit.  Allow embedded nulls
	// as this can be convenient for writing scheme code.
	if (!scm_is_pair(svalue_list) and !scm_is_null(svalue_list))
		scm_wrong_type_arg_msg(subrname, pos, svalue_list, "a list of (string) values");

	std::vector<std::string> valist;
	SCM sl = svalue_list;
	pos = 2;
	while (scm_is_pair(sl)) {
		SCM svalue = SCM_CAR(sl);

		if (not scm_is_null(svalue)) {
			char * v = scm_to_utf8_string(svalue);
			valist.emplace_back(v);
		}
		sl = SCM_CDR(sl);
		pos++;
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
#if 0
AttentionValue * SchemeSmob::verify_av(SCM sav, const char *subrname, int pos)
{
	if (!SCM_SMOB_PREDICATE(SchemeSmob::cog_misc_tag, sav))
		scm_wrong_type_arg_msg(subrname, pos, sav, "opencog attention value");

	scm_t_bits misctype = SCM_SMOB_FLAGS(sav);
	if (COG_Value != misctype)
		scm_wrong_type_arg_msg(subrname, pos, sav, "opencog attention value");

	AttentionValue *av = (AttentionValue *) SCM_SMOB_DATA(sav);
	return av;
}

/**
 * Return association list holding contents of an attention value
 */
SCM SchemeSmob::ss_av_get_value (SCM s)
{
	AttentionValue *av = verify_av(s, "cog-av->alist");

	SCM sti = scm_from_short(av->getSTI());
	SCM lti = scm_from_short(av->getLTI());
	SCM vlti = scm_from_ushort(av->getVLTI());

	SCM ssti = scm_from_utf8_symbol("sti");
	SCM slti = scm_from_utf8_symbol("lti");
	SCM svlti = scm_from_utf8_symbol("vlti");
	scm_remember_upto_here_1(s);

	SCM rc = SCM_EOL;
	rc = scm_acons(svlti, vlti, rc);
	rc = scm_acons(slti, lti, rc);
	rc = scm_acons(ssti, sti, rc);
	return rc;
}
#endif

#endif /* HAVE_GUILE */
/* ===================== END OF FILE ============================ */
