/*
 * opencog/atomspace/Node.cc
 *
 * Copyright (C) 2008-2010 OpenCog Foundation
 * Copyright (C) 2002-2007 Novamente LLC
 * All Rights Reserved
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

#include <stdio.h>

#include <opencog/atomspace/AtomTable.h>
#include <opencog/atomspace/ClassServer.h>
#include <opencog/atomspace/Link.h>
#include <opencog/util/Logger.h>

#include "Node.h"

using namespace opencog;

void Node::init(const std::string& cname)
{
    if (not classserver().isA(_type, NODE))
    {
        throw InvalidParamException(TRACE_INFO,
            "Node - Invalid node type '%d' %s.",
            _type, classserver().getTypeName(_type).c_str());
    }
    _name = cname;
}

std::string Node::toShortString(const std::string& indent)
{
    std::string tmpname = _name;
    if (_name == "")
        tmpname = "#" + std::to_string(_uuid);

    std::string atname;
    if (_atomTable)
        atname = std::to_string(_atomTable->get_uuid());
    else
        atname = "NULL";

    std::string nam = indent +
        "(" + classserver().getTypeName(_type) +
        " \"" + tmpname + "\") ; [" +
        std::to_string(_uuid) + "][" + atname +"]\n";
    return nam;
}

std::string Node::toString(const std::string& indent)
{
    std::string tmpname = _name;
    if (_name == "")
        tmpname = "#" + std::to_string(_uuid);

    std::string answer = indent;

    answer += "(" + classserver().getTypeName(_type);

    answer += " \"" + tmpname + "\"";

    // Print the TV and AV only if its not the default.
    if (not getAttentionValue()->isDefaultAV())
        answer += " (av " +
             std::to_string(getAttentionValue()->getSTI()) + " " +
             std::to_string(getAttentionValue()->getLTI()) + " " +
             std::to_string(getAttentionValue()->getVLTI()) + ")";

    if (not getTruthValue()->isDefaultTV())
        answer += " " + getTruthValue()->toString();

    answer += ") ; [" +
            std::to_string(_uuid) + "][" +
            std::to_string(_atomTable? _atomTable->get_uuid() : -1) +
            "]\n";

    return answer;
}

bool Node::operator==(const Atom& other) const
{
    return (getType() == other.getType()) and
           (getName() == dynamic_cast<const Node&>(other).getName());
}
