/*
 * ExampleSCM.h
 *
 * Example wrapper for creating guile modules.
 */

#ifndef _OPENCOG_EXAMPLE_SCM_H
#define _OPENCOG_EXAMPLE_SCM_H

#include <opencog/guile/SchemeModule.h>

namespace opencog {

class ExampleSCM : public ModuleWrap
{
	protected:
		virtual void init(void);
	public:
		ExampleSCM(void);
};

}

extern "C" {
// This function will be called to initialize the module.
void opencog_example_init(void);
};

#endif // _OPENCOG_EXAMPLE_SCM_H
