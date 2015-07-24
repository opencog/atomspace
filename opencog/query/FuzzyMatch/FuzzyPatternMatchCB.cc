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
#include <opencog/util/numeric.h>
#include "FuzzyPatternMatchCB.h"

using namespace opencog;

#define DEBUG

FuzzyPatternMatchCB::FuzzyPatternMatchCB(AtomSpace* as, const HandleSeq& rl)
        : DefaultPatternMatchCB(as),
          reject_list(rl),
          num_links(as->get_num_links())
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
            find_starters(h, depth + 1, clause_idx, hp, rtn);
    }

    // Get the nodes that are not an instance nor a variable
    else
    {
        NodePtr np(NodeCast(hp));

        if (hp != Handle::UNDEFINED and np)
        {
            pat_size++;

            if ((np->getType() != VARIABLE_NODE) and
                (np->getName().find("@") == std::string::npos) and
                (std::find(reject_list.begin(), reject_list.end(), hp) == reject_list.end()))
            {
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
bool FuzzyPatternMatchCB::initiate_search(PatternMatchEngine* pme)
{
    // Find potential starters from the clause (the input pattern)
    const Handle& clause = _pattern->mandatory[0];
    find_starters(clause, 0, 0, Handle::UNDEFINED, potential_starters);

    // For removing duplicates, if any, form the list of potential starters,
    // as we want to have a different starters for each of the searches
    auto check_uniqueness = [](const Starter& s1, const Starter& s2)
    { return s1.uuid == s2.uuid; };

    auto sort_by_uuid = [](const Starter& s1, const Starter& s2)
    { return s1.uuid < s2.uuid; };

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
    size_t num_starters = potential_starters.size();
    while (num_starters > search_cnt)
    {
        const Handle& starter_term = potential_starters[search_cnt].term;
        const Handle& best_start = potential_starters[search_cnt].handle;
        search_cnt++;

#ifdef DEBUG
        std::cout << "\n========================================\n";
        std::cout << "Initiating the fuzzy match... (" << search_cnt << "/"
                  << num_starters << ")\n";
        std::cout << "Starter:\n" << best_start->toShortString() << "\n";
        std::cout << "Start term:\n" << starter_term->toShortString();
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

            pme->explore_neighborhood(clause, starter_term, h);
        }
    }

    // Let's end the search here if there are solutions, continue could be costly
    if (solns.size() > 0)
    {
        std::cout << "Fuzzy match is finished.\n";
        return true;
    }

    // Return false to use other methods to find matches
    else return false;
}

/**
 * Implement the clause_match callback.
 *
 * @param ph  The pattern
 * @param gh  The potential solution found by the pattern matcher
 * @return    Always return true???
 */
bool FuzzyPatternMatchCB::clause_match(const Handle& ph, const Handle& gh)
{
    std::pair<UUID, UUID> p = std::make_pair(ph.value(), gh.value());

    // Avoid comparing the same pair of atoms again
    if (std::find(prev_compared.begin(), prev_compared.end(), p) == prev_compared.end())
    {
        check_if_accept(ph, gh);
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
    HandleSeq pn = get_all_nodes(ph);
    HandleSeq gn = get_all_nodes(gh);

    // Reject if the potential solution contains any atoms in the reject list
    for (const Handle& rh : reject_list)
        if (std::find(gn.begin(), gn.end(), rh) != gn.end()) return;

    // How many nodes the potential solution has in common with the pattern
    HandleSeq common_nodes;
    std::sort(pn.begin(), pn.end());
    std::sort(gn.begin(), gn.end());
    std::set_intersection(pn.begin(), pn.end(), gn.begin(), gn.end(),
                          std::back_inserter(common_nodes));
    size_t common = common_nodes.size();

    // The size different between the pattern and the potential solution
    size_t diff = std::abs((int)(pat_size - gn.size()));

    // Estimate how "rare" the common nodes are, by employing something like TFIDF
    double rareness = 0;
    for (const Handle& common_node : common_nodes)
    {
        // No. of times it appears in the pattern over the size of the pattern
        double f1 = (double) std::count(pn.begin(), pn.end(), common_node) / pn.size();

        // Total no. of links in the atomspace over the size of its incoming set
        // TODO: Maybe better to use TV for this estimation
        double f2 = log2(num_links / common_node->getIncomingSetSize());

        rareness += f1 * f2;
    }

    // A rough estimation of how similar they are
    double similarity = (10 * rareness) + common - diff;

#ifdef DEBUG
    std::cout << "\n========================================\n";
    std::cout << "Compaing:\n" << ph->toShortString() << "--- and:\n"
              << gh->toShortString() << "\n";
    std::cout << "Common nodes = " << common << "\n";
    std::cout << "Size diff = " << diff << "\n";
    std::cout << "Rareness = " << rareness << "\n";
    std::cout << "Similarity = " << similarity << "\n";
    std::cout << "Most similar = " << max_similarity << "\n";
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
