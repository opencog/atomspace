/*
 * FuzzyPatternMatch.cc
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

#include <opencog/atoms/pattern/PatternLink.h>
#include <opencog/atomutils/AtomUtils.h>
#include <opencog/atomutils/FindUtils.h>

#include "FuzzyPatternMatch.h"

using namespace opencog;

FuzzyPatternMatch::FuzzyPatternMatch(AtomSpace* as)
        : DefaultPatternMatchCB(as)
{
}

/**
 * Override the find_starters method in InitiateSearchCB. It examines the
 * pattern and find the starters that can be used to initiate fuzzy-searches.
 *
 * @param hp          The pattern (the hypergraph in the query)
 * @param depth       The depth of the starter in the pattern
 * @param clause_idx  The index of the clause, i.e. which clause the starter
 *                    is in among all input clauses
 * @param term        The term that the starter is located in the pattern
 * @param rtn         A list of potential starters found in the pattern
 */
void FuzzyPatternMatch::find_starters(const Handle& hp, const size_t& depth,
                                      const size_t& clause_idx,
                                      const Handle& term,
                                      std::set<Starter>& rtn)
{
    if (nullptr == hp) return;

    // Traverse its outgoing set if it is a link
    LinkPtr lp(LinkCast(hp));
    if (lp) {
        for (Handle h : lp->getOutgoingSet())
            find_starters(h, depth + 1, clause_idx, hp, rtn);
        return;
    }

    // Get the nodes that are not an instance nor a variable
    NodePtr np(NodeCast(hp));

    if (np->getType() != VARIABLE_NODE and
        np->getName().find("@") == std::string::npos)
    {
        Starter sn;
        sn.handle = hp;
        sn.term = term;
        sn.width = hp->getIncomingSetSize();
        sn.depth = depth;

        rtn.insert(sn);
    }
}

// Sort the starters by their "width" and "depth"
bool FuzzyPatternMatch::Starter::operator<(const Starter& s2) const
{
    if (width == s2.width) return depth > s2.depth;
    else return width < s2.width;
};

/**
 * Override the initiate_search method in the InitiateSearchCB. It begins
 * by finding one or more starter nodes, and then initiates a fuzzy-search
 * for each of the starter nodes until all of them have been used.
 *
 * @param pme  The PatternMatchEngine object
 * @return     True if one or more solutions are found, false otherwise
 */
bool FuzzyPatternMatch::initiate_search(PatternMatchEngine* pme)
{
    // Find starters from the clause
    std::set<Starter> starters;
    const Handle& clause = _pattern->mandatory[0];
    find_starters(clause, 0, 0, clause, starters);

    // Start the searches
    size_t search_cnt = 0;
    size_t num_starters = starters.size();
    auto iter = starters.begin();
    while (num_starters > search_cnt) {
        const Handle& starter_term = iter->term;
        const Handle& best_start = iter->handle;
        search_cnt++;
        iter++;

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

    // Let's end the search here, continue could be costly
    std::cout << "Fuzzy match is finished.\n";
    return true;
}

/**
 * Implement the link_match method in the Pattern Matcher. Compare and estimate
 * the similarity between the pattern and the potential solution, and decide
 * whether or not to accept it by calling accept_solution. The potential
 * solution will be accepted if it has a similarity score greater than or equals
 * to the maximum similarity score that we know, rejected otherwise.
 *
 * @param pl  A link from the pattern
 * @param gl  A possible grounding link
 * @return    True to accept all kinds of links when we are on the way to
 *            finding a solution, false when we have decided whether or not
 *            to accept it as a solution
 */
bool FuzzyPatternMatch::link_match(const LinkPtr& pl, const LinkPtr& gl)
{
    // In this case, gl is a potential solution
    if (_pattern->mandatory[0] == pl->getHandle()) {
        Handle pat = _pattern->mandatory[0];
        Handle soln = gl->getHandle();

        // Skip it if it's grounded by itself
        if (soln == pat)
            return false;

        // Skip it if we have seen it before
        if (prev_compared.find(soln) != prev_compared.end())
            return false;
        else prev_compared.insert(soln);

        accept_solution(pat, soln);

        // Returns false here to skip the rest of the pattern matching procedures,
        // including the permutation comparsion for unordered links, as we have
        // already decided whether or not to accpet this grounding.
        return false;
    }

    return true;
}

/**
 * Estimate how similar the potential solution and the input pattern are.
 *
 * @param pat   The input pattern
 * @param soln  The potential solution
 */
void FuzzyPatternMatch::accept_solution(const Handle& pat, const Handle& soln)
{
    // Find out how many nodes it has in common with the pattern
    HandleSeq common_nodes;
    HandleSeq pat_nodes = get_all_nodes(pat);
    HandleSeq soln_nodes = get_all_nodes(soln);

    std::sort(pat_nodes.begin(), pat_nodes.end());
    std::sort(soln_nodes.begin(), soln_nodes.end());

    std::set_intersection(pat_nodes.begin(), pat_nodes.end(),
                          soln_nodes.begin(), soln_nodes.end(),
                          std::back_inserter(common_nodes));

    // The size different between the pattern and the potential solution
    size_t diff = std::abs((int)(pat_nodes.size() - soln_nodes.size()));

    double similarity = 0;

    // Roughly estimate how "rare" each node is by using 1 / incoming set size
    // TODO: May use Truth Value instead
    for (const Handle& common_node : common_nodes)
        similarity += 1.0 / common_node->getIncomingSetSize();

    LAZY_LOG_FINE << "\n========================================\n"
                  << "Compaing:\n" << pat->toShortString()
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

/**
 * Implement the "cog-fuzzy-match" scheme primitive.
 * It uses the Pattern Matcher to find hypergraphs in the atomspace that are
 * similar to the query hypergraph, and returns the most similar ones.
 *
 * @param as  The atomspace that we are using
 * @param hp  The query hypergraph
 * @return    One or more similar hypergraphs
 */
Handle opencog::find_approximate_match(AtomSpace* as, const Handle& hp)
{
    FuzzyPatternMatch fpm(as);

    HandleSeq terms;
    terms.push_back(hp);

    std::set<Handle> no_vars;

    PatternLinkPtr slp(createPatternLink(no_vars, terms));
    slp->satisfy(fpm);

    LAZY_LOG_FINE << "---------- solns ----------";
    for (Handle h : fpm.get_solns())
        LAZY_LOG_FINE << h->toShortString();

    // Wrap the solutions in a ListLink and return it
    Handle gl = as->add_link(LIST_LINK, fpm.get_solns());
    return gl;
}
