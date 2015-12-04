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
#include <opencog/atomutils/FindUtils.h>
#include "FuzzyPatternMatchCB.h"

using namespace opencog;

FuzzyPatternMatchCB::FuzzyPatternMatchCB(AtomSpace* as, Type rt, const HandleSeq& excl)
        : DefaultPatternMatchCB(as),
          rtn_type(rt),
          excl_list(excl)
{
}

/**
 * Find the starters that can be used to initiate a fuzzy-search. Currently the
 * starters has to be a node that is not an instance nor a variable. It can't be
 * any node that is listed in the exclude-list either.
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
    if (lp) {
        for (Handle h : lp->getOutgoingSet())
            find_starters(h, depth + 1, clause_idx, hp, rtn);
    }

    // Get the nodes that are not an instance nor a variable
    else {
        NodePtr np(NodeCast(hp));

        if (hp and np) {
            if ((np->getType() != VARIABLE_NODE) and
                (np->getName().find("@") == std::string::npos) and
                (std::find(excl_list.begin(), excl_list.end(), hp) == excl_list.end())) {
                Starter sn;
                sn.uuid = hp.value();
                sn.handle = hp;
                sn.term = term;
                sn.width = hp->getIncomingSetSize();
                sn.depth = depth;

                rtn.push_back(sn);
            }
        }
    }
}

/**
 * Implement the initiate_search method in the Pattern Matcher. It finds and
 * initiates multiple searches using differnt nodes as starters.
 * It stops when each of the starters have been used for searching.
 *
 * @param pme   The PatternMatchEngine object
 * @return      True if one or more solutions are found, false otherwise
 */
bool FuzzyPatternMatchCB::initiate_search(PatternMatchEngine* pme)
{
    // Find starters from the clause
    std::vector<Starter> starters;
    const Handle& clause = _pattern->mandatory[0];
    find_starters(clause, 0, 0, clause, starters);

    // For removing duplicates, if any, form the list of starters,
    // as we want to have a different starters for each of the searches
    std::sort(starters.begin(), starters.end(),
              [](const Starter& s1, const Starter& s2)
              { return s1.uuid < s2.uuid; });

    starters.erase(std::unique(starters.begin(), starters.end(),
                               [](const Starter& s1, const Starter& s2)
                               { return s1.uuid == s2.uuid; }), starters.end());

    // Sort the starters by their "width" and "depth"
    auto sort_by_wd = [](const Starter& s1, const Starter& s2) {
        if (s1.width == s2.width) return s1.depth > s2.depth;
        else return s1.width < s2.width;
    };

    std::sort(starters.begin(), starters.end(), sort_by_wd);

    // Start the searches
    size_t search_cnt = 0;
    size_t num_starters = starters.size();
    while (num_starters > search_cnt) {
        const Handle& starter_term = starters[search_cnt].term;
        const Handle& best_start = starters[search_cnt].handle;
        search_cnt++;

        LAZY_LOG_FINE << "\n========================================\n"
                      << "Initiating the fuzzy match... ("
                      << search_cnt << "/"
                      << num_starters << ")\n"
                      << "Starter:\n" << best_start->toShortString() << "\n"
                      << "Start term:\n" << starter_term->toShortString()
                      << "========================================\n";

        IncomingSet iset = best_start->getIncomingSet();
        size_t iset_size = iset.size();
        for (size_t i = 0; i < iset_size; i++) {
            Handle h(iset[i]);

            LAZY_LOG_FINE << "Loop candidate ("
                          << (i + 1) << "/" << iset_size << "):\n"
                          << h->toShortString() << "\n";

            pme->explore_neighborhood(clause, starter_term, h);
        }
    }

    // Let's end the search here if there are solutions, continue could be costly
    if (solns.size() > 0) {
        std::cout << "Fuzzy match is finished.\n";
        return true;
    }

    // Return false to use other methods to find matches
    else return false;
}

/**
 * Compare and estimate the similarity between the pattern and the potential
 * solution, and decide whether or not to accept it. The potential solution
 * will be accepted if it has a similarity greater than or equals to the
 * maximum similarity that we know, rejected otherwise.
 *
 * @param var_soln   Groundings for the variables & links
 * @param term_soln  Groundings for the clauses
 * @return           Always returns false to search for more solutions
 */
bool FuzzyPatternMatchCB::grounding(const std::map<Handle, Handle>& var_soln,
                                    const std::map<Handle, Handle>& term_soln)
{
    for (auto i = term_soln.begin(); i != term_soln.end(); i++) {
        Handle soln = i->second;

        if (soln == _pattern->mandatory[0])
            continue;

        // Skip it if we have seen it before
        if (std::find(prev_compared.begin(), prev_compared.end(), soln.value())
                                                        != prev_compared.end())
            continue;
        else prev_compared.push_back(soln.value());

        // Skip if it is not the type of atoms we are looking for
        if (rtn_type and soln->getType() != rtn_type)
            continue;

        // Skip if it is or it contains some atom that we don't want
        if (std::any_of(excl_list.begin(), excl_list.end(),
                [&](Handle& h) { return is_atom_in_tree(soln, h); }))
            continue;

        // Find out how many nodes it has in common with the pattern
        HandleSeq common_nodes;
        HandleSeq pat_nodes = get_all_nodes(_pattern->mandatory[0]);
        HandleSeq soln_nodes = get_all_nodes(soln);
        size_t pat_size = pat_nodes.size();

        std::sort(pat_nodes.begin(), pat_nodes.end());
        std::sort(soln_nodes.begin(), soln_nodes.end());

        std::set_intersection(pat_nodes.begin(), pat_nodes.end(),
                              soln_nodes.begin(), soln_nodes.end(),
                              std::back_inserter(common_nodes));

        // The size different between the pattern and the potential solution
        size_t diff = std::abs((int)(pat_size - soln_nodes.size()));

        double similarity = 0;

        // Roughly estimate how "rare" each node is by using 1 / incoming set size
        // TODO: May use Truth Value instead
        for (const Handle& common_node : common_nodes)
            similarity += 1.0 / common_node->getIncomingSetSize();

        LAZY_LOG_FINE << "\n========================================\n"
                      << "Compaing:\n" << _pattern->mandatory[0]->toShortString()
                      << "--- and:\n" << soln->toShortString() << "\n"
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

    return false;
}
