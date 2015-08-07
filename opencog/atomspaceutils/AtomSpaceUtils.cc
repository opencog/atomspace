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

#include <opencog/atomspace/atom_types.h>
#include <opencog/atomspace/Link.h>
#include "AtomSpaceUtils.h"

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
            name += alphanum[rand() % (sizeof(alphanum) - 1)];
        }
        result = as.get_handle(t, name);
    } while (as.is_valid_handle(result));

    return as.add_node(t, name);
}

bool remove_hypergraph(AtomSpace& as, Handle h)
{
    LinkPtr link(LinkCast(h));

    // Recursive case
    if (link) {
        HandleSeq oset = link->getOutgoingSet();
        bool success = as.remove_atom(h);
        if (success)
            for (const Handle& oh : oset)
                success &= remove_hypergraph(as, oh);
        return success;
    }
    // Base case
    else {
        return as.remove_atom(h);
    }
}

} // namespace opencog
