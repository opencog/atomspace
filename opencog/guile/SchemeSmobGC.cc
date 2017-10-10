/*
 * SchemeSmobGC.cc
 *
 * Scheme small objects (SMOBS) garbage-collection methods
 *
 * Copyright (c) 2008,2009,2014 Linas Vepstas <linas@linas.org>
 */

#include <cstddef>
#include <libguile.h>

#include <opencog/truthvalue/TruthValue.h>
#include <opencog/guile/SchemePrimitive.h>
#include <opencog/guile/SchemeSmob.h>

using namespace opencog;

SCM SchemeSmob::mark_misc(SCM misc_smob)
{
	scm_t_bits misctype = SCM_SMOB_FLAGS(misc_smob);

	switch (misctype)
	{
		case COG_PROTOM: // Nothing to do here ...
		case COG_AS: // Nothing to do here ...
		case COG_LOGGER: // Nothing to do here ...
		case COG_EXTEND: // Nothing to do here ...
			return SCM_BOOL_F;

		// I don't get it .. started seeing these recently. I'm just
		// going to silently ignore thse, for now, don't know what
		// they mean. XXX TODO figure out and fix if needed. Or document.
		case 0:
			return SCM_BOOL_F;

		default:
			fprintf(stderr, "Error: opencog-guile: "
			        "don't know how to mark this type: %d\n",
			        (int) misctype);
			break;
	}

	return SCM_BOOL_F;
}

/**
 * Free the memory associated with an opencog guile object.
 * This routine is called by the guile garbage collector, from time to
 * time. For testing purposes, you can force the garbage collector to
 * run by saying (gc), while, for stats, try (gc-stats) and
 * (gc-live-object-stats). The later should show both "opencog-handle"
 * and "opencog-misc" stats.
 */
size_t SchemeSmob::free_misc(SCM node)
{
	scm_t_bits misctype = SCM_SMOB_FLAGS(node);

	switch (misctype)
	{
		case COG_AS:
		{
			AtomSpace *as = (AtomSpace *) SCM_SMOB_DATA(node);
			release_as(as);
			scm_remember_upto_here_1(node);
			return 0;
		}

		case COG_PROTOM:
			ProtoAtomPtr* pap;
			pap = (ProtoAtomPtr*) SCM_SMOB_DATA(node);
			delete pap;
			scm_remember_upto_here_1(node);
			return 0;

		case COG_EXTEND:
			PrimitiveEnviron *pe;
			pe = (PrimitiveEnviron *) SCM_SMOB_DATA(node);
			delete pe;
			scm_remember_upto_here_1(node);
			return 0;

		case COG_LOGGER:
			Logger* lgr;
			lgr = (Logger*) SCM_SMOB_DATA(node);
			release_logger(lgr);
			scm_remember_upto_here_1(node);
			return 0;

		default:
			fprintf(stderr, "Error: opencog-guile: "
			        "don't know how to free this type: %d\n",
			        (int) misctype);
			break;
	}
	return 0;
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

#ifdef HAVE_GUILE_2_2

	// Deal with a regression in guile-2.1.x See bug report
	// https://debbugs.gnu.org/cgi/bugreport.cgi?bug=25387
	scm_display (scm_from_utf8_string (str.c_str()), port);
#else
	scm_puts (str.c_str(), port);
#endif // HAVE_GUILE_2_2

	return 1; //non-zero means success
}

/* ===================== END OF FILE ============================ */
