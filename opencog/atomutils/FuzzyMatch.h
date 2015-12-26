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

    virtual bool accept_starter(const NodePtr&) = 0;
    virtual bool note_match(const Handle&, int depth) = 0;
    virtual HandleSeq finished_search(void) = 0;

private:
    void find_starters(const Handle& hg, const int& depth);
    void explore(const LinkPtr&, int);
};

} // namespace opencog
#endif  // FUZZY_MATCH_H
