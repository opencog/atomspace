/*
 * opencog/atoms/grounded/PythonRunner.cc
 *
 * Copyright (C) 2009, 2013, 2014, 2015, 2020 Linas Vepstas
 * SPDX-License-Identifier: AGPL-3.0-or-later
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
 */

#include <opencog/atoms/value/Value.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/cython/PythonEval.h>

#include <opencog/atoms/grounded/DLPython.h>
#include <opencog/atoms/grounded/PythonRunner.h>

// Avoid cryptic fails due to users forcing broken configurations.
#ifndef HAVE_CYTHON
#error "Error: The Makefiles or CMakefiles are misconfigured, and failed to detect Cython correctly."
#endif

using namespace opencog;

PythonRunner::PythonRunner(std::string s)
	: _fname(s)
{
}

// ----------------------------------------------------------

/// `execute()` -- evaluate a PythonRunner with arguments.
/// Execution happens in the scratch space.
///
/// Expects "args" to be a ListLink. These arguments
///     will be substituted into the predicate.
///
ValuePtr PythonRunner::execute(AtomSpace* scratch,
                               const ValuePtr& vargs,
                               bool silent)
{
	// XXX FIXME: update the python apply_v to accept LinkValue
	// so that we can pass python functions Values, not just Atoms.
	if (not vargs->is_atom())
		throw SyntaxException(TRACE_INFO,
			"PythonRunner: Expecting Handle; got %s",
			vargs->to_string().c_str());

	Handle asargs = HandleCast(vargs);
	if (asargs->is_executable())
	{
		ValuePtr vp(asargs->execute(scratch));
		if (not vp->is_atom())
			throw SyntaxException(TRACE_INFO,
				"PythonRunner: Expecting Handle; got %s after execing %s",
				vp->to_string().c_str(),
				asargs->to_string().c_str());

		asargs = HandleCast(vp);
	}
	asargs = scratch->add_atom(asargs);

	PythonEval* applier = get_evaluator_for_python(scratch);
	ValuePtr vp(applier->apply_v(scratch, _fname, asargs));
	return vp;
}
