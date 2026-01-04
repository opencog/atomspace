/*
 * opencog/atoms/grounded/SCMRunner.cc
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

#include <opencog/atoms/atom_types/atom_types.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/guile/SchemeEval.h>
#include <opencog/guile/SchemeSmob.h>
#include <opencog/atoms/value/VoidValue.h>

#include <opencog/atoms/grounded/SCMRunner.h>
#include "DLScheme.h"

using namespace opencog;

SCMRunner::SCMRunner(std::string s)
	: _fname(s)
{
}

// ----------------------------------------------------------

/// `execute()` -- evaluate a SCMRunner with arguments.
///
/// Expects "args" to be a ListLink. These arguments will be
///     substituted into the predicate.
///
/// The arguments are "eager-evaluated", because it is assumed that
/// the GPN is unaware of the concept of lazy evaluation, and can't
/// do it itself. The arguments are then inserted into the predicate,
/// and the predicate as a whole is then evaluated.
///
ValuePtr SCMRunner::execute(AtomSpace* base_as,
                            AtomSpace* scratch,
                            const ValuePtr& vargs,
                            bool silent)
{
	ValuePtr asargs = vargs;
	if (vargs->is_atom())
	{
		const Handle& hargs(HandleCast(vargs));
		if (hargs->is_executable())
			asargs = hargs->execute(scratch);
		else
			asargs = scratch->add_atom(hargs);
	}

	SchemeEval* applier = get_evaluator_for_scheme(scratch);
	ValuePtr vp(applier->apply_v(_fname, asargs));

	// The Scheme evaluator uses a fluid to hold "the current
	// atomspace", and this fluid is typically set to the scratch
	// space (in this thread). Set it back to "what it was", which
	// should be the provided base_as.
	//
	// Thus happens during the recursive lollapalooza tested in
	// MultiAtomSpaceUTest, and also trips in a few other unit
	// tests.
	SchemeEval* ev = get_evaluator_for_scheme(base_as);
	ev->set_atomspace(AtomSpaceCast(base_as));

	// In general, we expect the scheme fuction to return some Value.
	// But user-written functions can return anything, e.g. scheme
	// expressions. These are converted by scm_to_protom() into
	// null pointers. Here, we convert them to VoidValue.
	if (nullptr == vp) return createVoidValue();

	return vp;
}
