/**
 * atom_types.cc
 *
 * Generic Atom Types declaration
 *
 * Copyright (c) 2009, 2014 Linas Vepstas <linasvepstas@gmail.com>
 */

// To use this file, include it in another `atom_types_init.cc` file,
// which sets up the needed definition of INHERITANCE_FILE and of
// INITNAME and any required include files.

// Set constructor priority to 101 because we want this to be
// the very first one that runs, before any of the other type
// declarations run, and certainly before the factories run.
//
// As of March 2023, Ubuntu 22.04 Jammy LTS is not honoring these
// priorities. Code below hacks around this. Buggy combination:
//    gcc (Ubuntu 11.3.0-1ubuntu1~22.04) 11.3.0
//    GNU ld (GNU Binutils for Ubuntu) 2.38
//    ldd (Ubuntu GLIBC 2.35-0ubuntu3.1) 2.35
//
#ifdef LOAD_ME_FIRST
#define CTOR_PRIO (101)
#else
#define CTOR_PRIO (102)
#endif

#ifndef LOAD_ME_FIRST
extern "C" {
void base_types_init(void);
};
#endif

// Library initialization
static __attribute__ ((constructor CTOR_PRIO)) void init(void)
{
#ifndef LOAD_ME_FIRST
	base_types_init();
#endif

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
//
// See notes above about Ubuntu Jammy. Some 2023-era linkers seem to
// be bungling the constructor order. Thus, belt and suspenders:
// explicitly call the shared library ctor, just in case it got called
// in the wrong order. Sheesh.
//
void INITNAME(void)
{
	init();
}
};
