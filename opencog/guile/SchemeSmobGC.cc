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
