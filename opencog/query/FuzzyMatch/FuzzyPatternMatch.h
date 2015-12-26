/*
 * FuzzyPatternMatch.h
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

#ifndef FUZZYPATTERNMATCH_H
#define FUZZYPATTERNMATCH_H

namespace opencog
{
/**
 * The fuzzy pattern matcher searches for trees which are similar but
 * not identical to the input target pattern. This is done by examining
 * all possible trees that have at least one leaf node in common with
 * the target pattern.  A similarity score is assigned to each such
 * tree, and the ones with the highest scores are returned.
 *
 * It can be called from C++ via find_approximate_match(), or from Scheme
 * via (cog-fuzzy-match).
 */
class FuzzyPatternMatch
{
    public:
        FuzzyPatternMatch(const Handle&);
        HandleSeq get_solns() { return solns; }
        void initiate_search();

    private:
        void explore(const LinkPtr&, size_t);
        void accept_solution(const Handle&);

        // What we are matching
        Handle target;

        HandleSeq target_nodes;

        // The solutions
        HandleSeq solns;

        // Potential starters for the search
        struct Starter
        {
            Handle handle;
            size_t width;
            size_t depth;
            bool operator<(const Starter&) const;
        };

        // The minimum difference between the pattern and all the known solutions
        size_t min_size_diff = SIZE_MAX;

        // The maximum similarity of all the potential solutions we found
        double max_similarity = -std::numeric_limits<double>::max();

        void find_starters(const Handle& hg, const size_t& depth,
                           std::set<Starter>& rtn);
};

Handle find_approximate_match(AtomSpace* as, const Handle& hp);


}

#endif  // FUZZYPATTERNMATCH_H

