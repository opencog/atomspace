/*
 * opencog/atoms/base/Node.cc
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

#include <iomanip>

#include <opencog/atoms/atom_types/NameServer.h>

#include "Node.h"

using namespace opencog;

void Node::init()
{
    if (not nameserver().isA(_type, NODE))
    {
        throw InvalidParamException(TRACE_INFO,
            "Node - Invalid node type '%d' %s.",
            _type, nameserver().getTypeName(_type).c_str());
    }
}

/// Return a universally-unique string for each distinct node.
/// It needs to be fast (because it is used in performance-critical
/// code paths), it needs to be human-readable, and it must not have
/// any trailing newlines.
std::string Node::to_short_string(const std::string& indent) const
{
    size_t len = _name.length();
    std::string answer;
    answer.reserve(2*len);
    answer = indent + '(' + nameserver().getTypeName(_type) + " \"";
    for (unsigned int i=0; i < len; i++)
    {
        if ('"' == _name[i] or '\\' == _name[i])
        {
            answer += '\\';
            answer += _name[i];
        }
        else if ((unsigned char) _name[i] < 0x20)
        {
            // Characters that control printing.
            if ('\a' == _name[i]) answer += "\a";
            else if ('\b' == _name[i]) answer += "\\b";
            else if ('\t' == _name[i]) answer += "\\t";
            else if ('\n' == _name[i]) answer += "\\n";
            else if ('\v' == _name[i]) answer += "\\v";
            else if ('\f' == _name[i]) answer += "\\f";
            else if ('\r' == _name[i]) answer += "\\r";
            else answer += _name[i];
        }
        else
            answer += _name[i];
    }
    answer += '\"';

    // Print the TV only if its not the default.
    if (not getTruthValue()->isDefaultTV())
        answer += ' ' + getTruthValue()->to_string();

    answer += ')';
    return answer;
}

/// Return a universally-unique string for each distinct node.
/// It needs to be fast (because it is used in performance-critical
/// code paths), it needs to escape embedded quotes, and it must
/// not have any trailing newlines.
///
std::string Node::to_string_esc() const
{
    std::stringstream ss;

    ss << "(" << nameserver().getTypeName(_type) << " "
       << std::quoted(_name) << ")";

    return ss.str();
}

std::string Node::to_string(const std::string& indent) const
{
    return to_short_string(indent) + " ; " + id_to_string();
}

bool Node::operator==(const Atom& other) const
{
    // If other points to this, then have equality.
    if (this == &other) return true;

    // Rule out obvious mis-matches, based on the hash.
    if (get_hash() != other.get_hash()) return false;

    if (get_type() != other.get_type()) return false;
    return get_name() == other.get_name();
}

bool Node::operator<(const Atom& other) const
{
    if (this == &other) return false;

    ContentHash cht = get_hash();
    ContentHash cho = other.get_hash();
    if (cht != cho) return cht < cho;

    // We get to here only if the hashes are equal.
    // Compare the contents directly, for this
    // (hopefully rare) case.
    if (get_type() == other.get_type())
        return get_name() < other.get_name();
    else
        return get_type() < other.get_type();
}

ContentHash Node::compute_hash() const
{
	ContentHash hsh = std::hash<std::string>()(get_name());

	// 1<<43 - 369 is a prime number.
	hsh += (hsh<<5) + ((1UL<<43)-369) * get_type();

	// Nodes will never have the MSB set.
	ContentHash mask = ~(((ContentHash) 1UL) << (8*sizeof(ContentHash) - 1));
	hsh &= mask;

	if (Handle::INVALID_HASH == hsh) hsh -= 1;
	return hsh;
}
