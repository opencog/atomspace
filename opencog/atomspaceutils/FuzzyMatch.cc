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

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atomutils/AtomUtils.h>
#include <opencog/atomutils/FindUtils.h>

#include "FuzzyMatch.h"

using namespace opencog;

/**
 * Examines the pattern and find the starting leaves that can be
 * used to initiate fuzzy-searches.
 *
 * @param hp          The pattern (the hypergraph in the query)
 * @param depth       The depth of the starter in the pattern
 */
void FuzzyMatch::find_starters(const Handle& hp, const size_t& depth)
{
    // Traverse its outgoing set if it is a link
    LinkPtr lp(LinkCast(hp));
    if (lp) {
        for (const Handle& h : lp->getOutgoingSet()) {
            find_starters(h, depth + 1);
        }
        return;
    }

    // Get the nodes that are not an instance nor a variable
    NodePtr np(NodeCast(hp));

    if (np->getType() != VARIABLE_NODE and
        np->getName().find("@") == std::string::npos)
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
    }
}

/**
 * Find leaves at which a search can be started.
 */
HandleSeq FuzzyMatch::perform_search(const Handle& targ)
{
    target = targ;
    target_nodes = get_all_nodes(target);
    std::sort(target_nodes.begin(), target_nodes.end());

    // Find starting leaves from which to begin matches.
    find_starters(target, 0);

    return solns;
}

void FuzzyMatch::explore(const LinkPtr& gl,
                                size_t depth)
{
	if (0 < depth)
	{
		for (const LinkPtr& lptr : gl->getIncomingSet())
		{
			explore(lptr, depth-1);
		}
		return;
	}

	Handle soln(gl->getHandle());
	if (soln == target) return;

	accept_solution(soln);
}

/**
 * Estimate how similar the potential solution and the target pattern are.
 *
 * @param soln  The potential solution
 */
void FuzzyMatch::accept_solution(const Handle& soln)
{
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
}

/**
 * Implement the "cog-fuzzy-match" scheme primitive.
 * It finds hypergraphs in the atomspace that are similar to
 * the target hypergraph, and returns the most similar ones.
 *
 * @param as  The atomspace that we are using
 * @param hp  The query hypergraph
 * @return    One or more similar hypergraphs
 */
Handle opencog::find_approximate_match(AtomSpace* as, const Handle& hp)
{
    FuzzyMatch fpm;
    HandleSeq solns = fpm.perform_search(hp);

    LAZY_LOG_FINE << "---------- solns ----------";
    for (const Handle& h : solns)
        LAZY_LOG_FINE << h->toShortString();

    // Wrap the solutions in a ListLink and return it
    Handle gl = as->add_link(LIST_LINK, solns);
    return gl;
}
