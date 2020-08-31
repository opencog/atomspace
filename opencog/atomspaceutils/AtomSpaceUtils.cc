/*
 * AtomSpaceUtils.cc
 *
 * Copyright (C) 2014 OpenCog Foundation
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

#include <opencog/atoms/atom_types/atom_types.h>
#include <opencog/atoms/base/Link.h>
#include "AtomSpaceUtils.h"

#include <opencog/util/mt19937ar.h>

namespace opencog {

Handle add_prefixed_node(AtomSpace& as, Type t, const std::string& prefix)
{
    static const char alphanum[] =
        "0123456789"
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        "abcdefghijklmnopqrstuvwxyz";
    static const int len = 8;
    std::string name;
    Handle result;
    // Keep trying random suffixes until a non-existant name is generated.
    do {
        name = prefix;
        for (int i = 0; i < len; ++i) {
            name += alphanum[randGen().randint() % (sizeof(alphanum) - 1)];
        }
        result = as.get_handle(t, std::move(std::string(name)));
    } while (as.is_valid_handle(result));

    return as.add_node(t, std::move(name));
}

/// Return true if all of h was removed.
bool do_hypergraph_removal(AtomSpace& as, const Handle& h, bool from_storage)
{
    // Recursive case
    if (h->is_link()) {
        HandleSeq oset = h->getOutgoingSet();
        // bool success = (from_storage)? as.remove_atom(h) : as.extract_atom(h);
        bool success = as.extract_atom(h);
        if (success) {
            // Return true only if entire subgraph was removed.
            for (const Handle& oh : oset)
                if (not do_hypergraph_removal(as, oh, from_storage))
                    success = false;
        }
        return success;
    }
    // Base case
    else {
        // return (from_storage)? as.remove_atom(h) : as.extract_atom(h);
        return as.extract_atom(h);
    }
}

bool remove_hypergraph(AtomSpace& as, const Handle& h)
{
    return do_hypergraph_removal(as, h, true);
}

bool extract_hypergraph(AtomSpace& as, const Handle& h)
{
    return do_hypergraph_removal(as, h, false);
}
} // namespace opencog
