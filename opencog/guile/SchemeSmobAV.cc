/*
 * SchemeSmobAV.c
 *
 * Scheme small objects (SMOBS) for attention values.
 *
 * Copyright (c) 2008,2009 Linas Vepstas <linas@linas.org>
 */

#include <cstddef>
#include <libguile.h>

#include <opencog/atoms/truthvalue/AttentionValue.h>
#include <opencog/guile/SchemeSmob.h>

using namespace opencog;

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
	return protom_to_scm(ValueCast(av));
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

AttentionValuePtr SchemeSmob::verify_av(SCM sav, const char *subrname, int pos)
{
	ValuePtr pa(scm_to_protom(sav));
	AttentionValuePtr av(AttentionValueCast(pa));

	if (nullptr == av)
		scm_wrong_type_arg_msg(subrname, pos, sav, "opencog attention value");

	return av;
}

/* ===================== END OF FILE ============================ */
