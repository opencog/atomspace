/*
 * ExampleSCM.h
 *
 * Simplified wrapper for creating guile modules.
 * Copyright (c) 2008, 2014, 2015 Linas Vepstas <linas@linas.org>
 */

#ifndef _OPENCOG_EXAMPLE_SCM_H
#define _OPENCOG_EXAMPLE_SCM_H

#include <opencog/guile/SchemeModule.h>

namespace opencog {

class ExampleSCM : public ModuleWrap
{
	protected:
		virtual void init(void);
		static std::vector<FunctionWrap*> _binders;
	public:
		ExampleSCM(void);
		~ExampleSCM();
};

}

extern "C" {
// This function will be called to initialize the module.
void opencog_example_init(void);
};

#endif // _OPENCOG_EXAMPLE_SCM_H
