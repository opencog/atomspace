/*
 * FuzzyPatternMatchCB.cc
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

#include <opencog/atomutils/AtomUtils.h>
#include "FuzzyPatternMatchCB.h"

using namespace opencog;

//#define DEBUG

FuzzyPatternMatchCB::FuzzyPatternMatchCB(AtomSpace* as)
	: DefaultPatternMatchCB(as)
{
}

/**
 * Find the starters that can be used to initiate a fuzzy-search. Currently the
 * starters has to be a node that is not an instance nor a variable.
 *
 * @param hp          The pattern (the hypergraph in the query)
 * @param depth       The depth of the starter in the pattern
 * @param clause_idx  The index of the clause, i.e. which clause the starter
 *                    is in among all input clauses
 * @param term        The term that the starter is located in the pattern
 * @param rtn         A list of potential starters found in the pattern
 */
void FuzzyPatternMatchCB::find_starters(const Handle& hp, const size_t& depth,
                                        const size_t& clause_idx,
                                        const Handle& term,
                                        std::vector<Starter>& rtn)
{
    // Traverse its outgoing set if it is a link
    LinkPtr lp(LinkCast(hp));
    if (lp)
    {
        for (Handle h : lp->getOutgoingSet())
        {
            // Blow past the QuoteLinks
            if (QUOTE_LINK == h->getType()) h = LinkCast(h)->getOutgoingAtom(0);

            find_starters(h, depth + 1, clause_idx, hp, rtn);
        }
    }

    // Get the nodes that are not an instance nor a variable
    else
    {
        NodePtr np(NodeCast(hp));

        if (hp != Handle::UNDEFINED and np)
        {
            pat_size++;

            if ((np->getType() != VARIABLE_NODE) and
                (np->getName().find("@") == std::string::npos))
            {
                Starter sn;
                sn.uuid = hp.value();
                sn.handle = hp;
                sn.term = term;
                sn.clause_idx = clause_idx;
                sn.width = hp->getIncomingSetSize();
                sn.depth = depth;

                rtn.push_back(sn);
            }

            else if (np->getType() == VARIABLE_NODE) var_size++;
        }
    }
}

/**
 * Implement the neighbor_search method in the Pattern Matcher. The main
 * different between this method and the default one is that this initiates
 * multiple searches using differnt nodes as starters instead of one,
 * explores the neighborhood of each of them, and captures the partial
 * matches in the callbacks. It stops when there are no more available
 * starters in the pattern, or the number of searches it has done
 * equals to MAX_SEARCH.
 *
 * @param pme   The PatternMatchEngine object
 * @param vars  Variables for the Pattern Matcher
 * @param pat   The pattern we are looking for
 * @return      True if one or more solutions are found, false otherwise
 */
bool FuzzyPatternMatchCB::neighbor_search(PatternMatchEngine* pme,
                                          const Variables& vars,
                                          const Pattern& pat)
{
    init(vars, pat);

    // Find potential starters from all the clauses
    const HandleSeq& clauses = pat.mandatory;
    for (size_t i = 0; i < clauses.size(); i++)
    {
        // Skip evaluatable clause
        if (0 < pat.evaluatable_holders.count(clauses[i])) continue;

        find_starters(clauses[i], 0, i, Handle::UNDEFINED, potential_starters);
    }

    // For removing duplicates, if any, form the list of potential starters,
    // as we want to have a different starters for each of the searches
    auto check_uniqueness = [](const Starter& s1, const Starter& s2)
    {
        return s1.uuid == s2.uuid;
    };

    auto sort_by_uuid = [](const Starter& s1, const Starter& s2)
    {
        return s1.uuid < s2.uuid;
    };

    std::sort(potential_starters.begin(), potential_starters.end(), sort_by_uuid);
    potential_starters.erase(std::unique(potential_starters.begin(),
                                         potential_starters.end(),
                                         check_uniqueness),
                             potential_starters.end());

    // Sort the potential starters according to their "width" and "depth"
    auto sort_by_wd = [](const Starter& s1, const Starter& s2)
    {
        if (s1.width == s2.width) return s1.depth > s2.depth;
        else return s1.width < s2.width;
    };

    std::sort(potential_starters.begin(), potential_starters.end(), sort_by_wd);

    // Start the searches
    size_t search_cnt = 0;
    while (MAX_SEARCHES > search_cnt)
    {
        if (potential_starters.size() == search_cnt)
        {
#ifdef DEBUG
            std::cout << "No more available starters for the neighbor search.\n";
#endif
            break;
        }

        _root = clauses[potential_starters[search_cnt].clause_idx];
        _starter_term = potential_starters[search_cnt].term;
        const Handle& best_start = potential_starters[search_cnt].handle;
        search_cnt++;

#ifdef DEBUG
        std::cout << "\n========================================\n";
        std::cout << "Initiating the fuzzy match... (" << search_cnt << "/" <<
                     MAX_SEARCHES << ")\n";
        std::cout << "Starter:\n" << best_start->toShortString() << "\n";
        std::cout << "Start term:\n" << _starter_term->toShortString();
        std::cout << "========================================\n\n";
#endif

        IncomingSet iset = best_start->getIncomingSet();
        size_t iset_size = iset.size();
        for (size_t i = 0; i < iset_size; i++)
        {
            Handle h(iset[i]);

#ifdef DEBUG
            std::cout << "Loop candidate (" << (i + 1) << "/" << iset_size << "):\n"
                         << h->toShortString() << "\n";
#endif

            pme->explore_neighborhood(_root, _starter_term, h);
        }
    }

    // Let's end the search here if there are solutions, continue could be costly
    if (solns.size() > 0)
    {
        std::cout << "Fuzzy match is finished.\n";
        return true;
    }

    // Return false to use other methods to find matches
    else
    {
        _search_fail = true;
        return false;
    }
}


/**
 * Implement the link_match callback.
 *
 * It compares and estimates the similarity between the two input links, if they
 * haven't been compared previously.
 *
 * @param pLink  A link in the pattern
 * @param gLink  A link in the potential solution
 * @return       Always return true to accept it, and keep the matching going
 */
bool FuzzyPatternMatchCB::link_match(const LinkPtr& pLink, const LinkPtr& gLink)
{
    // Avoid comparing the same pair of atoms again, this can also avoid
    // giving duplicated solutions
    std::pair<UUID, UUID> p = std::make_pair(pLink->getHandle().value(),
                                             gLink->getHandle().value());
    if (std::find(prev_compared.begin(), prev_compared.end(), p) == prev_compared.end())
    {
        check_if_accept(pLink->getHandle(), gLink->getHandle());
        prev_compared.push_back(p);
    }

    return true;
}

/**
 * Compare and estimate the similarity between the two inputs, and decide
 * whether or not to accept it. The potential solution will be accepted if
 * it has a similarity greater than or equals to the maximum similarity that we
 * know, rejected otherwise.
 *
 * @param ph  The pattern
 * @param gh  The potential solution
 */
void FuzzyPatternMatchCB::check_if_accept(const Handle& ph, const Handle& gh)
{
    HandleSeq pnodes = getAllNodes(ph);
    HandleSeq gnodes = getAllNodes(gh);

    // Reject the candidate if it contains any VariableNode
    auto check_vars = [](const Handle& h)
    {
        return h->getType() == VARIABLE_NODE;
    };

    if (std::find_if(gnodes.begin(), gnodes.end(), check_vars) != gnodes.end())
        return;

    // Estimate the similarity by comparing how many nodes the potential
    // solution has in common with the pattern, also the number of extra and
    // missing nodes in it will also be taken in consideration
    HandleSeq common_nodes;
    std::sort(pnodes.begin(), pnodes.end());
    std::sort(gnodes.begin(), gnodes.end());
    std::set_intersection(pnodes.begin(), pnodes.end(),
                          gnodes.begin(), gnodes.end(),
                          std::back_inserter(common_nodes));

    // A rough estimation
    double common = 1.0 * (common_nodes.size() + var_size) / pat_size;
    double diff = 0.5 * std::abs((int) pat_size - (int) gnodes.size()) /
                  (pat_size + gnodes.size());
    similarity = common - diff;

#ifdef DEBUG
    std::cout << "\n========================================\n";
    std::cout << "Compaing:\n" << ph->toShortString() << "--- and:\n"
              << gh->toShortString() << "\n";
    std::cout << "Common nodes = " << common << "\n";
    std::cout << "Missing/Extra nodes = " << diff << "\n";
    std::cout << "similarity = " << similarity << "\n";
    std::cout << "max_similarity = " << max_similarity << "\n";
    std::cout << "========================================\n\n";
#endif

    // Decide if we should accept the potential solutions or not
    if (similarity > max_similarity)
    {
        max_similarity = similarity;
        solns.clear();
        solns.push_back(gh);
    }

    else if (similarity == max_similarity) solns.push_back(gh);

    return;
}

