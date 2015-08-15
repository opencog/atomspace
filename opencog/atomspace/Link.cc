/*
 * opencog/atomspace/Link.cc
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
#include <opencog/atomspace/Node.h>
#include <opencog/util/exceptions.h>
#include <opencog/util/Logger.h>

#include "Link.h"

//#define DPRINTF printf
#define DPRINTF(...)

using namespace opencog;

struct HandleComparison
{
    bool operator()(const Handle& h1, const Handle& h2) const {
        return (Handle::compare(h1, h2) < 0);
    }
};

void Link::resort(void)
{
    std::sort(_outgoing.begin(), _outgoing.end(), HandleComparison());
}

void Link::init(const std::vector<Handle>& outgoingVector)
	throw (InvalidParamException)
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

std::string Link::toShortString(std::string indent)
{
    std::stringstream answer;
    std::string more_indent = indent + "  ";

    answer << indent << "(" << classserver().getTypeName(_type);
    answer << " " << getTruthValue()->toString() << "\n";

    // Here the target string is made. If a target is a node, its name is
    // concatenated. If it's a link, all its properties are concatenated.
    for (const Handle& h : _outgoing) {
        if (h.operator->() != NULL)
            answer << h->toShortString(more_indent);
        else
            answer << indent << "Undefined Atom!\n";
    }

    answer << indent << ") ; [" << _uuid << "]\n";
    return answer.str();
}

std::string Link::toString(std::string indent)
{
    std::string answer;
    std::string more_indent = indent + "  ";
#define BUFSZ 1024
    static char buf[BUFSZ];

    snprintf(buf, BUFSZ, "(%s (av %d %d %d) %s\n",
             classserver().getTypeName(_type).c_str(),
             (int)getAttentionValue()->getSTI(),
             (int)getAttentionValue()->getLTI(),
             (int)getAttentionValue()->getVLTI(),
             getTruthValue()->toString().c_str());
    answer = indent + buf;
    // Here the targets string is made. If a target is a node, its name is
    // concatenated. If it's a link, all its properties are concatenated.
    for (const Handle& h : _outgoing) {
        if (h.operator->() != NULL)
            answer += h->toString(more_indent);
        else
            answer += indent + "Undefined Atom!\n";
    }

    answer += indent + ") ; [" + 
            std::to_string(_uuid).c_str() + "]\n";
    return answer;
}

bool Link::isSource(Handle handle) const throw (InvalidParamException)
{
    // On ordered links, only the first position in the outgoing set is a source
    // of this link. So, if the handle given is equal to the first position,
    // true is returned.
    Arity arity = getArity();
    if (classserver().isA(_type, ORDERED_LINK)) {
        return arity > 0 && _outgoing[0] == handle;
    } else if (classserver().isA(_type, UNORDERED_LINK)) {
        // If the link is unordered, the outgoing set is scanned, and the
        // method returns true if any position is equal to the handle given.
        for (Arity i = 0; i < arity; i++) {
            if (_outgoing[i] == handle) {
                return true;
            }
        }
        return false;
    } else {
        throw InvalidParamException(TRACE_INFO, "Link::isSource(Handle) unknown link type %d", _type);
    }
    return false;
}

bool Link::isSource(size_t i) const throw (IndexErrorException, InvalidParamException)
{
    // tests if the int given is valid.
    if (i > getArity()) {
        throw IndexErrorException(TRACE_INFO, "Link::isSource(size_t) invalid index argument");
    }

    // on ordered links, only the first position in the outgoing set is a source
    // of this link. So, if the int passed is 0, true is returned.
    if (classserver().isA(_type, ORDERED_LINK)) {
        return i == 0;
    } else if (classserver().isA(_type, UNORDERED_LINK)) {
        // on unordered links, the only thing that matters is if the int passed
        // is valid (if it is within 0..arity).
        return true;
    } else {
        throw InvalidParamException(TRACE_INFO, "Link::isSource(int) unknown link type %d", _type);
    }
}

bool Link::isTarget(Handle handle) const throw (InvalidParamException)
{
    // On ordered links, the first position of the outgoing set defines the
    // source of the link. The other positions are targets. So, it scans the
    // outgoing set from the second position searching for the given handle. If
    // it is found, true is returned.
    Arity arity = getArity();
    if (classserver().isA(_type, ORDERED_LINK)) {
        for (Arity i = 1; i < arity; i++) {
            if (_outgoing[i] == handle) {
                return true;
            }
        }
        return false;
    } else if (classserver().isA(_type, UNORDERED_LINK)) {
        // If the links is unordered, all the outgoing set is scanned.
        for (Arity i = 0; i < arity; i++) {
            if (_outgoing[i] == handle) {
                return true;
            }
        }
        return false;
    } else {
        throw InvalidParamException(TRACE_INFO, "Link::isTarget(Handle) unknown link type %d", _type);
    }
    return false;
}

bool Link::isTarget(size_t i) const throw (IndexErrorException, InvalidParamException)
{
    // tests if the int given is valid.
    if (i > getArity()) {
        throw IndexErrorException(TRACE_INFO, "Link::istarget(int) invalid index argument");
    }

    // on ordered links, the first position of the outgoing set defines the
    // source of the link. The other positions are targets.
    if (classserver().isA(_type, ORDERED_LINK)) {
        return i != 0;
    } else if (classserver().isA(_type, UNORDERED_LINK)) {
        // on unorderd links, the only thing that matter is if the position is
        // valid.
        return true;
    } else {
        throw InvalidParamException(TRACE_INFO, "Link::isTarget(int) unkown link type");
    }
    return false;
}

bool Link::operator==(const Atom& other) const
{
    if (getType() != other.getType()) return false;
    const Link& olink = dynamic_cast<const Link&>(other);

    Arity arity = getArity();
    if (arity != olink.getArity()) return false;
    for (Arity i = 0; i < arity; i++)
        if (_outgoing[i] != olink._outgoing[i]) return false;
    return true;
}

bool Link::operator!=(const Atom& other) const
{
    return !(*this == other);
}

