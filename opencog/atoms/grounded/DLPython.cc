/*
 * opencog/atoms/execution/DLPython.cc
 *
 * Copyright (C) 2019 Linas Vepstas
 * All Rights Reserved
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <dlfcn.h>
#include <mutex>
#include <opencog/util/exceptions.h>
#include "DLPython.h"

using namespace opencog;

PythonEval* opencog::get_evaluator_for_python(AtomSpace* as)
{
	typedef PythonEval* (*SEGetter)(AtomSpace*);
	static SEGetter getter = nullptr;
	if (getter) return getter(as);

	static std::mutex mtx;
	std::lock_guard<std::mutex> lock(mtx);

	static void* library = nullptr;
	if (nullptr == library) library = dlopen("libPythonEval.so", RTLD_LAZY);
	if (nullptr == library)
		throw RuntimeException(TRACE_INFO,
			"Unable to dynamically load libPythonEval.so: %s",
			dlerror());

	static void* getev = nullptr;
	if (nullptr == getev) getev = dlsym(library, "get_python_evaluator");
	if (nullptr == getev)
		throw RuntimeException(TRACE_INFO,
			"Unable to dynamically load Python evaluator: %s",
			dlerror());

	// static SEGetter getter = std::reinterpret_cast<SEGetter>(getev);
	getter = (SEGetter) getev;

	return getter(as);
}

static __attribute__ ((destructor)) void fini(void)
{
	// Don't bother. This can trigger a pointless error:
	//    Inconsistency detected by ld.so: dl-close.c: 811: _dl_close:
	//    Assertion `map->l_init_called' failed!
	// or we can just RTLD_NODELETE during the open, instead.
	// if (library) dlclose(library);
}
