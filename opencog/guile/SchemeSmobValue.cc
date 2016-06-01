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

#if 0
/* ============================================================== */

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
