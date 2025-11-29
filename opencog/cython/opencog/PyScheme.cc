/*
 * opencog/cython/PyScheme.cc
 *
 * Copyright (C) 2013 by OpenCog Foundation
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
#include "PyScheme.h"

#include <atomic>
#include <opencog/util/oc_assert.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/guile/SchemeEval.h>
#include <opencog/cython/executioncontext/Context.h>

using namespace opencog;

// Convenience wrapper, for stand-alone usage.
std::string opencog::eval_scheme(AtomSpace* as, const std::string &s)
{
#ifdef HAVE_GUILE
	OC_ASSERT(nullptr != as, "Cython failed to specify an atomspace!");
	SchemeEval* evaluator = SchemeEval::get_scheme_evaluator(as);
	evaluator->clear_pending();
	std::string scheme_return_value = evaluator->eval(s);

	// If there's an error, the scheme_return_value will contain
	// a backtrace.  Be sure to display that to the user.
	if (evaluator->eval_error())
		throw RuntimeException(TRACE_INFO,
		       "Python-Scheme Wrapper: Failed to execute '%s'\n%s",
		       s.c_str(), scheme_return_value.c_str());

	if (evaluator->input_pending())
		throw RuntimeException(TRACE_INFO,
		      "Python-Scheme Wrapper: Syntax error in input: '%s'", s.c_str());

	return scheme_return_value;
#else // HAVE_GUILE
	return "Error: Compiled without Guile support";
#endif // HAVE_GUILE
}

// Convenience wrapper, for stand-alone usage.
ValuePtr opencog::eval_scheme_v(AtomSpace* as, const std::string &s)
{
#ifdef HAVE_GUILE
	OC_ASSERT(nullptr != as, "Cython failed to specify an atomspace!");

	SchemeEval* evaluator = SchemeEval::get_scheme_evaluator(as);
	evaluator->clear_pending();
	ValuePtr scheme_return_value = evaluator->eval_v(s);

	if (evaluator->eval_error())
		throw RuntimeException(TRACE_INFO,
		       "Python-Scheme Wrapper: Failed to execute '%s'", s.c_str());

	return scheme_return_value;
#else // HAVE_GUILE
	return "Error: Compiled without Guile support";
#endif // HAVE_GUILE
}
