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

using namespace opencog;

static void do_init()
{
	// It should be enough to do this only once, instead of once
	// per thread; so this is a belt-and-suspenders strategy for
	// making sure python initialization doesn't fall down.
	static thread_local bool thread_is_inited = false;
	if (thread_is_inited) return;
	thread_is_inited = true;

	SchemeEval* evaluator = SchemeEval::get_evaluator(nullptr);
	evaluator->clear_pending();
	evaluator->eval(
		"(define cog-initial-as (cog-atomspace))"
		"(if (eq? cog-initial-as #f)"
		"	(begin "
		"		(set! cog-initial-as (cog-new-atomspace))"
		"		(cog-set-atomspace! cog-initial-as)))");
}

// Convenience wrapper, for stand-alone usage.
std::string opencog::eval_scheme(AtomSpace* as, const std::string &s)
{
#ifdef HAVE_GUILE
	do_init();
	OC_ASSERT(nullptr != as, "Cython failed to specify an atomspace!");
	SchemeEval* evaluator = SchemeEval::get_evaluator(as);
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
	do_init();
	OC_ASSERT(nullptr != as, "Cython failed to specify an atomspace!");

	SchemeEval* evaluator = SchemeEval::get_evaluator(as);
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

// Convenience wrapper, for stand-alone usage.
Handle opencog::eval_scheme_h(AtomSpace* as, const std::string &s)
{
#ifdef HAVE_GUILE
	do_init();
	OC_ASSERT(nullptr != as, "Cython failed to specify an atomspace!");

	SchemeEval* evaluator = SchemeEval::get_evaluator(as);
	evaluator->clear_pending();
	Handle scheme_return_value = evaluator->eval_h(s);

	if (evaluator->eval_error())
		throw RuntimeException(TRACE_INFO,
		       "Python-Scheme Wrapper: Failed to execute '%s'", s.c_str());

	return scheme_return_value;
#else // HAVE_GUILE
	return Handle();
#endif // HAVE_GUILE
}

// Convenience wrapper, for stand-alone usage.
AtomSpace* opencog::eval_scheme_as(const std::string &s)
{
#ifdef HAVE_GUILE
	do_init();
	SchemeEval* evaluator = SchemeEval::get_evaluator(nullptr);
	evaluator->clear_pending();
	const AtomSpacePtr& asp = evaluator->eval_as(s);

	if (nullptr == asp)
		throw RuntimeException(TRACE_INFO,
		       "Python-Scheme Wrapper: Null atomspace for '%s'", s.c_str());

	if (evaluator->eval_error())
		throw RuntimeException(TRACE_INFO,
		       "Python-Scheme Wrapper: Failed to execute '%s'", s.c_str());

	return asp.get();
#else // HAVE_GUILE
	return nullptr;
#endif // HAVE_GUILE
}
