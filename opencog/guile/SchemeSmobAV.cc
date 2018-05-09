/*
 * SchemeSmobAV.c
 *
 * Scheme small objects (SMOBS) for attention values.
 *
 * Copyright (c) 2008,2009 Linas Vepstas <linas@linas.org>
 */

#include <cstddef>
#include <libguile.h>

#include <opencog/truthvalue/AttentionValue.h>
#include <opencog/guile/SchemeSmob.h>

using namespace opencog;

/* ============================================================== */
/**
 * Search for an attention value in a list of values.
 * Return the attention value if found, else return null.
 * Throw errors if the list is not stictly just key-value pairs
 */
AttentionValuePtr SchemeSmob::get_av_from_list(SCM slist)
{
	while (scm_is_pair(slist))
	{
		SCM sval = SCM_CAR(slist);
		if (SCM_SMOB_PREDICATE(SchemeSmob::cog_misc_tag, sval))
		{
			scm_t_bits misctype = SCM_SMOB_FLAGS(sval);
			switch (misctype)
			{
				case COG_PROTOM: {
					ProtoAtomPtr pa(scm_to_protom(sval));
					AttentionValuePtr av(AttentionValueCast(pa));
					if (av) return av;
				}
				default:
					break;
			}
		}

		slist = SCM_CDR(slist);
	}

	return nullptr;
}

/* ============================================================== */

std::string SchemeSmob::av_to_string(const AttentionValuePtr& av)
{
#define BUFLEN 120
	char buff[BUFLEN];

	// We pretend that they are actually short integers.
	snprintf(buff, BUFLEN, "(av %f %f %f)",
	         av->getSTI(), av->getLTI(), av->getVLTI());

	return buff;
}

/* ============================================================== */

SCM SchemeSmob::av_to_scm (const AttentionValuePtr& av)
{
	return protom_to_scm(ProtoAtomCast(av));
}

/**
 * Create a new attention value, with indicated sti/lti/vlti
 */
SCM SchemeSmob::ss_new_av (SCM ssti, SCM slti, SCM svlti)
{
	if (!scm_is_real(ssti)) {
		scm_wrong_type_arg_msg("cog-new-av", 1, ssti, "double");
	}
	if (!scm_is_real(slti)) {
		scm_wrong_type_arg_msg("cog-new-av", 2, slti, "double");
	}
	if (!scm_is_real(svlti)) {
		scm_wrong_type_arg_msg("cog-new-av", 3, svlti, "double");
	}
	AttentionValue::sti_t sti = scm_to_double(ssti);
	AttentionValue::lti_t lti = scm_to_double(slti);
	AttentionValue::vlti_t vlti = scm_to_double(svlti);
	AttentionValuePtr av = AttentionValue::createAV(sti, lti, vlti);
	return av_to_scm(av);
}

/* ============================================================== */
/**
 * Return true if the scm is an attention value
 */
SCM SchemeSmob::ss_av_p (SCM s)
{
	ProtoAtomPtr pa(scm_to_protom(s));
	if (nullptr == pa) return SCM_BOOL_F;

	if (pa->get_type() == ATTENTION_VALUE)
		return SCM_BOOL_T;

	scm_remember_upto_here_1(s);
	return SCM_BOOL_F;
}

/* ============================================================== */

AttentionValuePtr SchemeSmob::verify_av(SCM sav, const char *subrname, int pos)
{
	ProtoAtomPtr pa(scm_to_protom(sav));
	AttentionValuePtr av(AttentionValueCast(pa));

	if (nullptr == av)
		scm_wrong_type_arg_msg(subrname, pos, sav, "opencog attention value");

	return av;
}

/**
 * Return association list holding contents of an attention value
 */
SCM SchemeSmob::ss_av_get_value (SCM s)
{
	AttentionValuePtr av = verify_av(s, "cog-av->alist");

	SCM sti =scm_from_double(av->getSTI());
	SCM lti = scm_from_double(av->getLTI());
	SCM vlti = scm_from_double(av->getVLTI());

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

/* ===================== END OF FILE ============================ */
