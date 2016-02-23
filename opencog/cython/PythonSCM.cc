/*
 * opencog/cython/PythonSCM.h
 *
 * Copyright (c) 2016 Linas Vepstas <linasvepstas@gmail.com>
 * All Rights Reserved
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the
 * exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public
 * License
 * along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#ifndef _OPENCOG_PYTHON_SCM_H
#define _OPENCOG_PYTHON_SCM_H

#include <string>

namespace opencog
{
/** \addtogroup grp_smob
 *  @{
 */

class PythonSCM
{
private:
	static void* init_in_guile(void*);
	static void init_in_module(void*);
	void init(void);
public:
	PythonSCM();
	const std::string& eval(const std::string&);
}; // class

/** @}*/
}  // namespace


extern "C" {
void opencog_python_init(void);
};

#endif // _OPENCOG_PYTHON_SCM_H

#include <opencog/guile/SchemePrimitive.h>
#include <opencog/cython/PythonEval.h>

using namespace opencog;

PythonSCM::PythonSCM()
{
#ifdef HAVE_GUILE
	static bool is_init = false;
	if (is_init) return;
	is_init = true;
	scm_with_guile(init_in_guile, this);
#endif
}

void* PythonSCM::init_in_guile(void* self)
{
#ifdef HAVE_GUILE
	scm_c_define_module("opencog python", init_in_module, self);
	scm_c_use_module("opencog python");
#endif
	return NULL;
}

void PythonSCM::init_in_module(void* data)
{
	PythonSCM* self = (PythonSCM*) data;
	self->init();
}

void PythonSCM::init(void)
{
#ifdef HAVE_GUILE
	define_scheme_primitive("python-eval", &PythonSCM::eval, this, "python");
#endif
}

const std::string& PythonSCM::eval(const std::string& pystr)
{
	// This assumes that python is already initialized. If not,
	// I guess it creahes and burns...
	static PythonEval& pyev = PythonEval::instance();

	// This assumes that python is being invoked from a single thread.
	static std::string rv;
	rv = pyev.eval(pystr);
	return rv;
}

void opencog_python_init(void)
{
	static PythonSCM patty;
}
