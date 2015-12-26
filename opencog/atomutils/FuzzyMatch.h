/*
 * FuzzyMatch.h
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

#ifndef FUZZY_MATCH_H
#define FUZZY_MATCH_H

#include <opencog/atomspace/Handle.h>
#include <opencog/atomspace/Link.h>
#include <opencog/atomspace/Node.h>

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
class FuzzyMatch
{
public:
    HandleSeq perform_search(const Handle&);
    virtual ~FuzzyMatch() {}

protected:
    // What we are matching
    Handle target;
    HandleSeq target_nodes;

    virtual bool accept_starter(const NodePtr&);
    virtual void accept_solution(const Handle&);

private:
    void explore(const LinkPtr&, size_t);

    // The solutions
    HandleSeq solns;

    // The minimum difference between the pattern and all
    // the known solutions
    size_t min_size_diff = SIZE_MAX;

    // The maximum similarity of all the potential solutions we found
    double max_similarity = -std::numeric_limits<double>::max();

    void find_starters(const Handle& hg, const size_t& depth);
};

} // namespace opencog
#endif  // FUZZY_MATCH_H
