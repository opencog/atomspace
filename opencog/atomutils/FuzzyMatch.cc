/*
 * FuzzyMatch.cc
 *
 * Copyright (C) 2015 OpenCog Foundation
 *
 * Author: Leung Man Hin <https://github.com/leungmanhin>
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

#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atomutils/FindUtils.h>

#include "FuzzyMatch.h"

using namespace opencog;

/**
 * Recursively explores the incoming set of a proposed tree until
 * either `try_match()` returns false or the "root" is reached.
 *
 * @param gl  A proposed tree
 */
void FuzzyMatch::explore(const LinkPtr& gl)
{
	Handle soln(gl->getHandle());
	bool look_for_more = try_match(soln);
	if (not look_for_more) return;

	for (const LinkPtr& lptr : gl->getIncomingSet())
		explore(lptr);
}

/**
 * Recursively explores the target pattern and proposes each subtree
 * as a possible starting point for a similarity search. If the subtree
 * is accepted as a starting point, then all trees sharing the subtree
 * are proposed as being similar.
 *
 * @param hp  A subtree of the target pattern.
 */
void FuzzyMatch::find_starters(const Handle& hp)
{
	if (accept_starter(hp))
	{
		LAZY_LOG_FINE << "\n========================================\n"
		              << "Initiating the fuzzy match... ("
		              << "Starter:\n" << hp->toShortString() << "\n"
		              << "========================================\n";

		for (const LinkPtr& lptr: hp->getIncomingSet())
		{
			LAZY_LOG_FINE << "Loop candidate"
			              << lptr->toShortString() << "\n";

			explore(lptr);
		}
		return;
	}

	// Proposed start was not accepted. Look farther down, at it's
	// sub-trees.
	LinkPtr lp(LinkCast(hp));
	if (lp) {
		for (const Handle& h : lp->getOutgoingSet()) {
			find_starters(h);
		}
	}
}

/**
 * Find leaves at which a search can be started.
 */
RankedHandleSeq FuzzyMatch::perform_search(const Handle& target)
{
	start_search(target);

	// Find starting atoms from which to begin matches.
	find_starters(target);

	// Give the derived class a chance to wrap things up.
	return finished_search();
}
