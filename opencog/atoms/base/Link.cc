/*
 * opencog/atoms/base/Link.cc
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

#include <opencog/util/exceptions.h>
#include <opencog/util/Logger.h>

#include <opencog/atoms/base/ClassServer.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atomspace/AtomTable.h>

#include "Link.h"

//#define DPRINTF printf
#define DPRINTF(...)

using namespace opencog;

void Link::resort(void)
{
    // Caution: this comparison function MUST BE EXACTLY THE SAME as
    // the one in AtomTable.cc, used for Unordered links. Changing
    // this without changing the other one will break things!
    std::sort(_outgoing.begin(), _outgoing.end(), handle_less());
}

void Link::init(const std::vector<Handle>& outgoingVector)
{
    if (not classserver().isA(_type, LINK)) {
        throw InvalidParamException(TRACE_INFO,
            "Link ctor: Atom type is not a Link: '%d' %s.",
            _type, classserver().getTypeName(_type).c_str());
    }

    _outgoing = outgoingVector;
    // If the link is unordered, it will be normalized by sorting the
    // elements in the outgoing list.
    if (classserver().isA(_type, UNORDERED_LINK)) {
        resort();
    }
}

Link::~Link()
{
    DPRINTF("Deleting link:\n%s\n", this->toString().c_str());
}

std::string Link::toShortString(const std::string& indent)
{
    std::stringstream answer;
    std::string more_indent = indent + "  ";

    answer << indent << "(" << classserver().getTypeName(_type);
    if (not getTruthValue()->isDefaultTV())
        answer << " " << getTruthValue()->toString();
    answer << "\n";

    // Here the target string is made. If a target is a node, its name is
    // concatenated. If it's a link, all its properties are concatenated.
    for (const Handle& h : _outgoing) {
        if (h.operator->() != NULL)
            answer << h->toShortString(more_indent);
        else
            answer << more_indent << "Undefined Atom!\n";
    }

    answer << indent << ") ; [" << _uuid << "]";

    if (_atomTable)
        answer << "[" << _atomTable->get_uuid() << "]\n";
    else
        answer << "[NULL]\n";

    return answer.str();
}

std::string Link::toString(const std::string& indent)
{
    std::string answer = indent;
    std::string more_indent = indent + "  ";

    answer += "(" + classserver().getTypeName(_type);

    // Print the TV and AV only if its not the default.
    if (not getAttentionValue()->isDefaultAV())
        answer += " (av " +
             std::to_string(getAttentionValue()->getSTI()) + " " +
             std::to_string(getAttentionValue()->getLTI()) + " " +
             std::to_string(getAttentionValue()->getVLTI()) + ")";

    if (not getTruthValue()->isDefaultTV())
        answer += " " + getTruthValue()->toString();

    answer += "\n";
    // Here, the outset string is made. If a target is a node,
    // its name is concatenated. If it's a link, then recurse.
    for (const Handle& h : _outgoing) {
        if (h.operator->() != NULL)
            answer += h->toString(more_indent);
        else
            answer += more_indent + "Undefined Atom!\n";
    }

    answer += indent + ") ; [" +
            std::to_string(_uuid) + "][" +
            std::to_string(_atomTable? _atomTable->get_uuid() : -1) +
            "]\n";

    return answer;
}

bool Link::operator==(const Atom& other) const
{
    if (getType() != other.getType()) return false;
    const Link& olink = dynamic_cast<const Link&>(other);

    Arity arity = getArity();
    if (arity != olink.getArity()) return false;
    for (Arity i = 0; i < arity; i++)
    {
        NodePtr tn(NodeCast(_outgoing[i]));
        NodePtr on(NodeCast(olink._outgoing[i]));
        if (tn and on and *tn != *on) return false;

        LinkPtr tl(LinkCast(_outgoing[i]));
        LinkPtr ol(LinkCast(olink._outgoing[i]));
        if (tl and ol and *tl != *ol) return false;
    }
    return true;
}

bool Link::operator!=(const Atom& other) const
{
    return !(*this == other);
}

bool Link::operator<(const Atom& other) const
{
    if (getType() == other.getType()) {
        const HandleSeq& outgoing = getOutgoingSet();
        const HandleSeq& other_outgoing = other.getOutgoingSet();
        Arity arity = outgoing.size();
        Arity other_arity = other_outgoing.size();
        if (arity == other_arity) {
            Arity i = 0;
            while (i < arity) {
                Handle ll = outgoing[i];
                Handle rl = other_outgoing[i];
                if (ll == rl)
                    i++;
                else
                    return ll->operator<(*rl.atom_ptr());
            }
            return false;
        } else
            return arity < other_arity;
    } else
        return getType() < other.getType();
}
