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
#include "atomspace_api.h"

using namespace opencog;

/**
 * This method should be defined in separate compilation unit otherwise
 * call_python_method() will be declared later then used.
 */
ValuePtr PythonGroundedObject::invoke(std::string const& method_name,
						AtomSpace* atomspace, ValuePtr const& _args)
{
	return call_python_method(object, method_name, atomspace, _args);
}

/**
 * This constructor loads Cython opencog.atomspace "cdef api" definitions, in
 * particulart call_python_method().
 */
static __attribute__ ((constructor)) void init(void)
{
	import_opencog__atomspace();
}
