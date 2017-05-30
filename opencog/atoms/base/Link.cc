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

#include <boost/range/algorithm.hpp>

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

void Link::init(const HandleSeq& outgoingVector)
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

std::string Link::toShortString(const std::string& indent) const
{
    std::stringstream answer;
    std::string more_indent = indent + "  ";

    answer << indent << "(" << classserver().getTypeName(_type);

    if (not getTruthValue()->isDefaultTV())
        answer << " " << getTruthValue()->toString();
    answer << "\n";

    // Here the target string is made. If a target is a node, its name is
    // concatenated. If it's a link, all its properties are concatenated.
    for (const Handle& h : _outgoing)
        answer << h->toShortString(more_indent);

    answer << indent << ")\n";

    return answer.str();
}

std::string Link::toString(const std::string& indent) const
{
    std::string answer = indent;
    std::string more_indent = indent + "  ";

    answer += "(" + classserver().getTypeName(_type);

    // Print the TV only if its not the default.
    if (not getTruthValue()->isDefaultTV())
        answer += " " + getTruthValue()->toString();

    answer += "\n";
    // Here, the outset string is made. If a target is a node,
    // its name is concatenated. If it's a link, then recurse.
    for (const Handle& h : _outgoing)
        answer += h->toString(more_indent);

    answer += indent + ") ; " + idToString() + "\n";

    return answer;
}

// Content-based comparison.
bool Link::operator==(const Atom& other) const
{
    // If other points to this, then have equality.
    if (this == &other) return true;

    // Rule out obvious mis-matches, based on the hash.
    if (get_hash() != other.get_hash()) return false;
    if (getType() != other.getType()) return false;

    Arity sz = getArity();
    if (sz != other.getArity()) return false;

    // Perform a content-compare on the outgoing set.
    const HandleSeq& rhs = other.getOutgoingSet();
    for (Arity i = 0; i < sz; i++)
    {
        if (*((AtomPtr)_outgoing[i]) != *((AtomPtr)rhs[i]))
            return false;
    }
    return true;
}

// Content-based ordering.
bool Link::operator<(const Atom& other) const
{
    if (get_hash() < other.get_hash()) return true;
    if (other.get_hash() < get_hash()) return false;

    // We get to here only if the hashes are equal.
    // Compare the contents directly, for this
    // (hopefully rare) case.
    if (getType() != other.getType())
        return getType() < other.getType();

    const HandleSeq& outgoing = getOutgoingSet();
    const HandleSeq& other_outgoing = other.getOutgoingSet();
    Arity arity = outgoing.size();
    Arity other_arity = other_outgoing.size();
    if (arity != other_arity)
        return arity < other_arity;

    for (Arity i=0; i < arity; i++)
    {
        const Handle& ll(outgoing[i]);
        const AtomPtr& rl(other_outgoing[i]);
        if (ll->operator!=(*rl))
            return ll->operator<(*rl);
    }
    return false;
}

/// Returns a Merkle tree hash -- that is, the hash of this link
/// chains the hash values of the child atoms, as well.
ContentHash Link::compute_hash() const
{
	// 1<<44 - 377 is prime
	ContentHash hsh = ((1UL<<44) - 377) * getType();
	for (const Handle& h: _outgoing)
	{
		hsh += (hsh <<5) + h->get_hash(); // recursive!
	}

	// Links will always have the MSB set.
	ContentHash mask = ((ContentHash) 1UL) << (8*sizeof(ContentHash) - 1);
	hsh |= mask;

	if (Handle::INVALID_HASH == hsh) hsh -= 1;
	_content_hash = hsh;
	return _content_hash;
}
