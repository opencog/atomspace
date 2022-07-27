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
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <opencog/atoms/value/Value.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/cython/PythonEval.h>

#include <opencog/atoms/grounded/DLPython.h>
#include <opencog/atoms/grounded/PythonRunner.h>

using namespace opencog;

PythonRunner::PythonRunner(std::string s)
	: _fname(s)
{
}

// ----------------------------------------------------------

/// `execute()` -- evaluate a PythonRunner with arguments.
///
/// Expects "args" to be a ListLink. These arguments will be
///     substituted into the predicate.
///
ValuePtr PythonRunner::execute(AtomSpace* as,
                               const Handle& cargs,
                               bool silent)
{
	// If we arrive here from queries or other places, the
	// argument will not be (in general) in any atomspace.
	// That's because it was constructed on the fly, and
	// we're trying to stick to lazy evaluation. But we have
	// draw the line here: the callee necesssarily expects
	// arguments to be in the atomspace. So we add now.
	Handle asargs = as->add_atom(cargs);

	PythonEval* applier = get_evaluator_for_python(as);

	return applier->apply_v(as, _fname, asargs);
}

ValuePtr PythonRunner::evaluate(AtomSpace* as,
                                const Handle& cargs,
                                bool silent)
{
	Handle asargs = as->add_atom(cargs);

	PythonEval* applier = get_evaluator_for_python(as);

	return CastToValue(applier->apply_tv(as, _fname, asargs));
}
