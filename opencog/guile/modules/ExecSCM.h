/*
 * ExecSCM.h
 *
 * Guile Scheme bindings for the execution links
 * Copyright (c) 2008, 2014, 2015 Linas Vepstas <linas@linas.org>
 */

#ifndef _OPENCOG_EXEC_SCM_H
#define _OPENCOG_EXEC_SCM_H

#ifdef HAVE_GUILE
#include <opencog/guile/SchemeModule.h>

namespace opencog {

class ExecSCM : public ModuleWrap
{
	protected:
		virtual void init(void);
		static std::vector<FunctionWrap*>* _binders;
	public:
		ExecSCM(void);
		~ExecSCM();
};

}

extern "C" {
void opencog_exec_init(void);
};

#endif // HAVE_GUILE
#endif // _OPENCOG_EXEC_SCM_H
