/*
 * SchemeSmobPrint.cc
 *
 * Scheme small objects (SMOBS) printing.
 *
 * Copyright (c) 2008,2009,2014,2020 Linas Vepstas <linas@linas.org>
 */

#include <libguile.h>

#include <opencog/guile/SchemePrimitive.h>
#include <opencog/guile/SchemeSmob.h>

using namespace opencog;

/* ============================================================== */

std::string SchemeSmob::misc_to_string(SCM node)
{
	scm_t_bits misctype = SCM_SMOB_FLAGS(node);
	switch (misctype)
	{
		case COG_PROTOM:
			return protom_to_string(node);

		case COG_EXTEND:
		{
			// return "#<opencog extension>\n";
			// Hmm. Is this really the right thing to return ?? I'm not sure ..
			PrimitiveEnviron * pe = (PrimitiveEnviron *) SCM_SMOB_DATA(node);
			std::string str(pe->get_name());
			scm_remember_upto_here_1(node);
			return str;
		}
		default:
			return "#<unknown opencog type>\n";
	}
	return "";
}

int SchemeSmob::print_misc(SCM node, SCM port, scm_print_state * ps)
{
	std::string str = misc_to_string(node);

#if defined(HAVE_GUILE_2_2) || defined(HAVE_GUILE3)

	// Deal with a regression in guile-2.1.x See bug report
	// https://debbugs.gnu.org/cgi/bugreport.cgi?bug=25397
	// Basically, scm_puts() stopped working, AND guile made
	// a huge design flaw in string handling. What a cock-up.
	// See nots on SchemeSmob::convert_to_utf8() for details.

	SCM scmstr = scm_c_catch(SCM_BOOL_T,
		(scm_t_catch_body) scm_from_utf8_string, (void *) str.c_str(),
		SchemeSmob::convert_to_utf8, (void *) str.c_str(), NULL, NULL);

	scm_display (scmstr, port);
#else
	scm_puts (str.c_str(), port);
#endif // HAVE_GUILE_2_2

	return 1; //non-zero means success
}

/* ===================== END OF FILE ============================ */
