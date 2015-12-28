/*
 * ChainerUtils.cc
 *
 * Copyright (C) 2014 OpenCog Foundation
 *
 * Author: William Ma <https://github.com/williampma>
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

#include <iostream>

#include <opencog/atomspace/Link.h>
#include <opencog/atomspace/Node.h>
#include "ChainerUtils.h"

namespace opencog
{

void get_outgoing_nodes(const Handle& hinput,
                        UnorderedHandleSet& node_set,
                        Type type)
{
    LinkPtr link(LinkCast(hinput));
    // Recursive case
    if (link) {
        for (const Handle& h : link->getOutgoingSet())
            get_outgoing_nodes(h, node_set, type);
        return;
    }

    // Base case
    if (NODE == type or // Empty means all kinds of nodes
        NodeCast(hinput)->getType() == type)
    {
        node_set.insert(hinput);
    }
}


/**
 * Makes a one to one similarity matching. If the atoms
 * are of type UnorderedLink, does one vs all similarity
 * matching and removes the matched from matching list
 * immediately.
 *
 * @param h1  A handle
 * @param h2  A handle
 * @param strict_type_match A flag telling how type matching should be
 * done.
 *
 * @return  A boolean true if similar and false otherwise.
 */
bool are_similar(const Handle& h1, const Handle& h2, bool strict_type_match)
{
    if (h1 == h2)
        return true;

    if (NodeCast(h1) and NodeCast(h2))
        return !strict_type_match or h1->getType() == h2->getType();

    LinkPtr lh1(LinkCast(h1));
    LinkPtr lh2(LinkCast(h2));

    if (lh1 and lh2) {
        if (strict_type_match and (lh1->getType() != lh2->getType()))
            return false;

        HandleSeq hseqh1 = lh1->getOutgoingSet();
        HandleSeq hseqh2 = lh2->getOutgoingSet();

        if (hseqh1.size() != hseqh2.size())
            return false;

        // Unordered links should be treated in a special way
        if (classserver().isA(lh1->getType(), UNORDERED_LINK) or classserver().isA(
                lh2->getType(), UNORDERED_LINK)) {

            for (const auto& h1 : hseqh1) {
                for (auto it = hseqh2.begin(); it != hseqh2.end(); ++it) {
                    if (are_similar(h1, h2, strict_type_match)) {
                        hseqh2.erase(it);
                        break;
                    }
                }
            }

            // Empty means all has been mapped. Success.
            return hseqh2.empty() or false;
        }

        for (HandleSeq::size_type i = 0; i < hseqh1.size(); i++) {
            if (not are_similar(hseqh1[i], hseqh2[i], strict_type_match))
                return false;
        }

        return true;
    }

    return false;
}

HandleSeq get_neighbors(const Handle& h, Type desiredLinkType)
{
    HandleSeq answer;
    for (const LinkPtr& link : h->getIncomingSet())
    {
        if (classserver().isA(link->getType(), UNORDERED_LINK)) continue;
        if (link->getOutgoingAtom(0) != h) continue;
        if (link->getType() != desiredLinkType) continue;

        for (const Handle& handle : link->getOutgoingSet()) {
           if (handle == h) continue;
           answer.emplace_back(handle);
        }
    }
    return answer;
}

/* Tail-recursive helper function. We mark it static, so that
 * gcc can optimize this, i.e. call it without buying the stack
 * frame. */
static void get_distant_neighbors_rec(const Handle& h,
                                      UnorderedHandleSet& res,
                                      int dist)
{
    res.insert(h);

    // Recursive calls
    if (dist != 0) {
        // 1. Fetch incomings
        for (const LinkPtr& in_l : h->getIncomingSet()) {
            Handle in_h = in_l->getHandle();
            if (res.find(in_h) == res.cend()) // Do not re-explore
                get_distant_neighbors_rec(in_h, res, dist - 1);
        }
        // 2. Fetch outgoings
        LinkPtr link = LinkCast(h);
        if (link) {
            for (const Handle& out_h : link->getOutgoingSet()) {
                if (res.find(out_h) == res.cend()) // Do not re-explore
                    get_distant_neighbors_rec(out_h, res, dist - 1);
            }
        }
    }
}

UnorderedHandleSet get_distant_neighbors(const Handle& h, int dist)
{
    UnorderedHandleSet results;
    get_distant_neighbors_rec(h, results, dist);
    results.erase(h);
    return results;
}

} // namespace OpenCog
