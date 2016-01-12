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


void FuzzyMatch::explore(const LinkPtr& gl, int depth)
{
	Handle soln(gl->getHandle());
	bool look_for_more = try_match(soln, depth);
	if (not look_for_more) return;

	for (const LinkPtr& lptr : gl->getIncomingSet())
	{
		explore(lptr, depth-1);
	}
}

/**
 * Recursively explores the target pattern and proposes each subtree
 * as a possible starting point for a similarity search. If the subtree
 * is accepted as a starting point, then all trees sharing thwe subtree
 * are proposed as being similar.
 *
 * @param hp          A subtree of the target pattern.
 * @param depth       The depth of the subtree in the pattern.
 */
void FuzzyMatch::find_starters(const Handle& hp, const int& depth)
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

			explore(lptr, depth-1);
		}
		return;
	}

	// Proposed start was not accepted. Look farther down, at it's
	// sub-trees.
	LinkPtr lp(LinkCast(hp));
	if (lp) {
		for (const Handle& h : lp->getOutgoingSet()) {
			find_starters(h, depth + 1);
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
	find_starters(target, 0);

	// Give the derived class a chance to wrap things up.
	return finished_search();
}
