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
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
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
ValuePtr SCMRunner::execute(AtomSpace* as,
                            const ValuePtr& vargs,
                            bool silent)
{
	ValuePtr asargs = vargs;

	// If we arrive here from queries or other places, the
	// argument will not be (in general) in any atomspace.
	// That's because it was constructed on the fly, and
	// we're trying to stick to lazy evaluation. But we have
	// draw the line here: the callee necessarily expects
	// arguments to be in the atomspace. So we add now.
	if (vargs->is_atom())
		asargs = as->add_atom(HandleCast(vargs));

	SchemeEval* applier = get_evaluator_for_scheme(as);
	AtomSpacePtr saved_as = applier->get_scheme_as();
	ValuePtr vp = applier->apply_v(_fname, asargs);
	if (saved_as)
		applier->set_scheme_as(saved_as);

	// In general, we expect the scheme fuction to return some Value,
	// maybe a TruthValue for predicates, or Atoms for Schemas. But
	// user-written functions can return anything, e.g. scheme
	// expressions. These are converted by scm_to_protom() into
	// null pointers. Here, we convert them to VoidValue.
	if (nullptr == vp) return createVoidValue();

	return vp;
}
