/*
 * SchemeSmobLogger.c
 *
 * Scheme small objects (SMOBS) for atom spaces.
 *
 * Copyright (c) 2017 OpenCog Foundation
 */

#include <cstddef>
#include <libguile.h>

#include <opencog/guile/SchemeSmob.h>
#include <opencog/util/Logger.h>
#include <opencog/util/oc_assert.h>

using namespace opencog;

/* ============================================================== */

std::string SchemeSmob::logger_to_string(const Logger *l)
{
#define BUFLEN 120
	char buff[BUFLEN];

	snprintf(buff, BUFLEN, "#<logger %p>", l);
	return buff;
}

SCM SchemeSmob::logger_to_scm(Logger* lg)
{
	SCM smob;
	SCM_NEWSMOB (smob, cog_misc_tag, lg);
	SCM_SET_SMOB_FLAGS(smob, COG_LOGGER);
	return smob;
}

/* ============================================================== */
/**
 * Return true if the scm is a logger
 */
SCM SchemeSmob::ss_logger_p(SCM s)
{
	if (SCM_SMOB_PREDICATE(SchemeSmob::cog_misc_tag, s))
	{
		scm_t_bits misctype = SCM_SMOB_FLAGS(s);
		switch (misctype)
		{
			case COG_LOGGER:
				return SCM_BOOL_T;

			default:
				return SCM_BOOL_F;
		}
	}
	return SCM_BOOL_F;
}

/* ============================================================== */
/* Cast SCM to logger */

Logger* SchemeSmob::ss_to_logger(SCM sl)
{
	if (not SCM_SMOB_PREDICATE(SchemeSmob::cog_misc_tag, sl))
		return nullptr;

	scm_t_bits misctype = SCM_SMOB_FLAGS(sl);
	if (COG_LOGGER != misctype)
		return nullptr;

	Logger* l = (Logger *) SCM_SMOB_DATA(sl);
	scm_remember_upto_here_1(sl);
	return l;
}

/* ============================================================== */

Logger* SchemeSmob::verify_logger(SCM sl, const char * subrname, int pos)
{
   Logger* l = ss_to_logger(sl);
   if (nullptr == l)
      scm_wrong_type_arg_msg(subrname, pos, sl, "opencog logger");

   return l;
}
