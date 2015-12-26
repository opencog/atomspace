/*
 * FuzzyMatchBasic.cc
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

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atomutils/AtomUtils.h>
#include <opencog/atomutils/FindUtils.h>

#include "FuzzyMatchBasic.h"

using namespace opencog;

bool FuzzyMatchBasic::accept_starter(const NodePtr& np)
{
	return (np->getType() != VARIABLE_NODE and
		np->getName().find("@") == std::string::npos);
}

/**
 * Estimate how similar the potential solution and the target pattern are.
 *
 * @param soln  The potential solution
 */
bool FuzzyMatchBasic::try_match(const Handle& soln, int depth)
{
	if (0 < depth) return true;
	if (0 > depth) return false;

	// Find out how many nodes it has in common with the pattern
	HandleSeq common_nodes;
	HandleSeq soln_nodes = get_all_nodes(soln);

	std::sort(soln_nodes.begin(), soln_nodes.end());

	std::set_intersection(target_nodes.begin(), target_nodes.end(),
	                      soln_nodes.begin(), soln_nodes.end(),
	                      std::back_inserter(common_nodes));

	// The size different between the pattern and the potential solution
	size_t diff = std::abs((int)target_nodes.size() - (int)soln_nodes.size());

	double similarity = 0.0;

	// Roughly estimate how "rare" each node is by using 1 / incoming set size
	// TODO: May use Truth Value instead
	for (const Handle& common_node : common_nodes)
		similarity += 1.0 / common_node->getIncomingSetSize();

	LAZY_LOG_FINE << "\n========================================\n"
	              << "Comparing:\n" << target->toShortString()
	              << "----- and:\n" << soln->toShortString() << "\n"
	              << "Common nodes = " << common_nodes.size() << "\n"
	              << "Size diff = " << diff << "\n"
	              << "Similarity = " << similarity << "\n"
	              << "Most similar = " << max_similarity << "\n"
	              << "========================================\n";

	// Decide if we should accept the potential solutions or not
	if ((similarity > max_similarity) or
		(similarity == max_similarity and diff < min_size_diff)) {
		max_similarity = similarity;
		min_size_diff = diff;
		solns.clear();
		solns.push_back(soln);
	}

	else if (similarity == max_similarity and diff == min_size_diff) {
		solns.push_back(soln);
	}

	return true;
}

/* No-op; we already build "solns", just return it. */
HandleSeq FuzzyMatchBasic::finished_search(void)
{
	return solns;
}
