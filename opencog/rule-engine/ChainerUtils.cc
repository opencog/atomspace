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

#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/base/Node.h>
#include "ChainerUtils.h"

namespace opencog
{

void get_outgoing_nodes(const Handle& hinput,
                        UnorderedHandleSet& node_set,
                        Type type)
{
    // Recursive case
    if (hinput->is_link()) {
        for (const Handle& h : hinput->getOutgoingSet())
            get_outgoing_nodes(h, node_set, type);
        return;
    }

    // Base case
    if (NODE == type or // Empty means all kinds of nodes
        hinput->get_type() == type)
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

    if (h1->is_node() and h2->is_node())
        return !strict_type_match or h1->get_type() == h2->get_type();

    if (h1->is_link() and h2->is_link()) {
        if (strict_type_match and (h1->get_type() != h2->get_type()))
            return false;

        const HandleSeq& hseqh1 = h1->getOutgoingSet();
        HandleSeq hseqh2 = h2->getOutgoingSet();

        if (hseqh1.size() != hseqh2.size())
            return false;

        // Unordered links should be treated in a special way
        if (nameserver().isA(h1->get_type(), UNORDERED_LINK) or
            nameserver().isA(h2->get_type(), UNORDERED_LINK))
        {
            for (const Handle& h1 : hseqh1) {
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

} // namespace OpenCog
