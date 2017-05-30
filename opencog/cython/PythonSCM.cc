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
#ifdef HAVE_GUILE

#ifndef _OPENCOG_PYTHON_SCM_H
#define _OPENCOG_PYTHON_SCM_H

#include <string>
#include <opencog/atomspace/AtomSpace.h>

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
	std::string eval(const std::string&);
	void apply_as(const std::string&, AtomSpace*);
}; // class

/** @}*/
}  // namespace


extern "C" {
void opencog_python_init(void);
};

#endif // _OPENCOG_PYTHON_SCM_H

// ===================================================================

#include <opencog/guile/SchemePrimitive.h>
#include <opencog/cython/PythonEval.h>

using namespace opencog;

PythonSCM::PythonSCM()
{
	static bool is_init = false;
	if (is_init) return;
	is_init = true;
	scm_with_guile(init_in_guile, this);
}

void* PythonSCM::init_in_guile(void* self)
{
	scm_c_define_module("opencog python", init_in_module, self);
	scm_c_use_module("opencog python");

	// Make sure that guile and python are using the same atomspace.
	// This will avoid assorted confusion.
	PythonEval::instance(SchemeSmob::ss_get_env_as("python-eval"));
	return NULL;
}

void PythonSCM::init_in_module(void* data)
{
	PythonSCM* self = (PythonSCM*) data;
	self->init();
}

void PythonSCM::init(void)
{
	define_scheme_primitive("python-eval", &PythonSCM::eval, this, "python");
	define_scheme_primitive("python-call-with-as", &PythonSCM::apply_as, this, "python");
}

std::string PythonSCM::eval(const std::string& pystr)
{
	static PythonEval& pyev = PythonEval::instance();
	return pyev.eval(pystr);
}

void PythonSCM::apply_as(const std::string& pystr, AtomSpace* as)
{
	static PythonEval& pyev = PythonEval::instance();

	pyev.apply_as(pystr, as);
}

void opencog_python_init(void)
{
	static PythonSCM patty;
}

#endif // HAVE_GUILE
