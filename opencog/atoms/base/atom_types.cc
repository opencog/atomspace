/**
 * atom_types.cc
 *
 * Generic Atom Types declaration
 *
 * Copyright (c) 2009, 2014 Linas Vepstas <linasvepstas@gmail.com>
 */

// library initialization
#if defined(WIN32) && defined(_DLL)
namespace win {
#include <windows.h>
}

win::BOOL APIENTRY DllMain(win::HINSTANCE hinstDLL,  // handle to DLL module
                           win::DWORD fdwReason,     // reason for calling function
                           win::LPVOID lpvReserved)  // reserved
{
    System::setModuleHandle(hinstDLL);
    switch (fdwReason) {
        case DLL_PROCESS_ATTACH:
            #include INHERITANCE_FILE
            #ifdef INHERITANCE_FILE2
            #include INHERITANCE_FILE2
            #endif
            break;
        case DLL_THREAD_ATTACH:
            break;
        case DLL_THREAD_DETACH:
            break;
        case DLL_PROCESS_DETACH:
            break;
    }
    return TRUE;
}
#elif __GNUC__


#define paste(s) opencog_shared_library_ ## s
#define boolname(s) paste(s)

// This *must* be a publically-visible global variable!
// The goal here is to use ODR (One Definition Rule) to avoid
// accidentally initializing types twice. This can happen when
// this shared library is linked to some other library, and also
// is dynamically loaded by scheme/guile during module loading.
// In particular, this currently afflicts nlp-types. I'm not
// sure if there is some better way to avoid double-loads.
// This works, so we're going with it.
bool boolname(INITNAME) = false;

static __attribute__ ((constructor)) void init(void)
{
    if (boolname(INITNAME)) return;
    boolname(INITNAME) = true;

    #include INHERITANCE_FILE
    #ifdef INHERITANCE_FILE2
    #include INHERITANCE_FILE2
    #endif
}

static __attribute__ ((destructor)) void fini(void)
{
}

#endif

extern "C" {
// Calling this forces this shared-lib to load, thus calling the 
// constructor above, thus causing the atom types to be loaded into
// the atomspace.
void INITNAME(void)
{
	/* No-op */
}
};
