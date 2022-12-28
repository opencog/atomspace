/**
 * atom_types.cc
 *
 * Generic Atom Types declaration
 *
 * Copyright (c) 2009, 2014 Linas Vepstas <linasvepstas@gmail.com>
 */

// Set constructor priority to 101 because we want this to be
// the very first one that runs, before any of the other type
// declarations run, and certainly before the factories run.
#ifdef LOAD_ME_FIRST
#define CTOR_PRIO (101)
#else
#define CTOR_PRIO (102)
#endif

// Library initialization
static __attribute__ ((constructor CTOR_PRIO)) void init(void)
{
#define str(x) #x
#define xstr(x) str(x)

	bool is_init = opencog::nameserver().beginTypeDecls(xstr(INITNAME));
	if (is_init) return;

	#include INHERITANCE_FILE
	#ifdef INHERITANCE_FILE2
	#include INHERITANCE_FILE2
	#endif
	opencog::nameserver().endTypeDecls();

#ifdef LOAD_ME_FIRST
	// Backwards compat. Argh...
	opencog::TYPE_SET_LINK = opencog::TYPE_INTERSECTION_LINK;
#endif
}

static __attribute__ ((destructor)) void fini(void)
{
}

extern "C" {
// Calling this forces this shared-lib to load, thus calling the 
// constructor above, thus causing the atom types to be loaded into
// the atomspace.
void INITNAME(void)
{
	/* No-op */
}
};
