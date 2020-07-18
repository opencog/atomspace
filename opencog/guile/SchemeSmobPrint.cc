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

/// If server_mode is true, then do things that a server would want;
/// otherwise, behave in a human-friendly interactive way.  Currently,
/// server-mode enables printing of 16-decimal-place truth values.
bool SchemeSmob::server_mode = false;

SCM SchemeSmob::ss_set_server_mode(SCM boo)
{
	bool old_mode = server_mode;
	server_mode = (SCM_BOOL_F != boo);
	return old_mode ? SCM_BOOL_T : SCM_BOOL_F;
}

/* ============================================================== */

std::string SchemeSmob::misc_to_string(SCM node)
{
	scm_t_bits misctype = SCM_SMOB_FLAGS(node);
	switch (misctype)
	{
		case COG_AS:
		{
			std::string str(as_to_string((AtomSpace *) SCM_SMOB_DATA(node)));
			scm_remember_upto_here_1(node);
			return str;
		}
		case COG_LOGGER:
		{
			std::string str(logger_to_string((Logger *) SCM_SMOB_DATA(node)));
			scm_remember_upto_here_1(node);
			return str;
		}
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
	scm_display (scm_from_utf8_string (str.c_str()), port);
#else
	scm_puts (str.c_str(), port);
#endif // HAVE_GUILE_2_2

	return 1; //non-zero means success
}

/* ===================== END OF FILE ============================ */
