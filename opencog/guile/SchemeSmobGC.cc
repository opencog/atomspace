/*
 * SchemeSmobGC.cc
 *
 * Scheme small objects (SMOBS) garbage-collection methods
 *
 * Copyright (c) 2008,2009,2014 Linas Vepstas <linas@linas.org>
 */

#include <libguile.h>

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
			ValuePtr* pap;
			pap = (ValuePtr*) SCM_SMOB_DATA(node);
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

/* ===================== END OF FILE ============================ */
