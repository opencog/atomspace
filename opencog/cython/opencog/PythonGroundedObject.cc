/*
 * opencog/cython/opencog/PythonGroundedObject.cc
 *
 * Copyright (C) 2019 by OpenCog Foundation
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

#include <string>

#include "PythonGroundedObject.h"

/*
 * Methods which uses atomspace_api.h header should be defined in separate
 * compilation unit otherwise call_python_method() will be declared later then
 * used.
 */
#include "atomspace_api.h"

using namespace opencog;

static void load_cython_proxies()
{
	if (!Py_IsInitialized())
		throw RuntimeException(TRACE_INFO, "Python is expected to be "
				"initialized when using Python specific part of API");
	import_opencog__atomspace();
}

PythonGroundedObject::PythonGroundedObject(void *object, bool unwrap_args)
	: object(object)
	, unwrap_args(unwrap_args)
{
	static int cython_proxies_loaded = 0;
	if (!cython_proxies_loaded)
	{
		load_cython_proxies();
		cython_proxies_loaded = 1;
	}
	Py_INCREF(this->object);
}

PythonGroundedObject::~PythonGroundedObject()
{
	Py_DECREF(object);
}

GroundedFunction PythonGroundedObject::get_method(std::string const& method_name)
{
	return std::bind(&PythonGroundedObject::invoke, this, method_name,
			std::placeholders::_1, std::placeholders::_2);
}

class PyLockGIL
{

private:
	PyGILState_STATE gstate;

public:
	PyLockGIL() : gstate(PyGILState_Ensure())
	{
	}

	~PyLockGIL()
	{
		PyGILState_Release(gstate);
	}
};

ValuePtr PythonGroundedObject::invoke(std::string const& method_name,
						AtomSpace* atomspace, ValuePtr const& args)
{
	PyLockGIL gil;
	return call_python_method(unwrap_args, object, method_name, atomspace, args);
}

