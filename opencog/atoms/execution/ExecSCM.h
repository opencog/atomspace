/*
 * ExecSCM.h
 *
 * Simplified wrapper for creating guile modules.
 * Copyright (c) 2008, 2014, 2015 Linas Vepstas <linas@linas.org>
 */

#ifndef _OPENCOG_EXEC_SCM_H
#define _OPENCOG_EXEC_SCM_H

#include <opencog/guile/SchemeModule.h>

namespace opencog {

class ExecSCM : public ModuleWrap
{
	protected:
		virtual void init(void);
		static std::vector<FunctionWrap*> _binders;
	public:
		ExecSCM(void);
		~ExecSCM();
};

}

extern "C" {
void opencog_exec_init(void);
};

#endif // _OPENCOG_EXEC_SCM_H
