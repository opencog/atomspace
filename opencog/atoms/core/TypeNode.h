/*
 * opencog/atoms/core/TypeNode.h
 *
 * Copyright (C) 2015 Linas Vepstas
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

#ifndef _OPENCOG_TYPE_NODE_H
#define _OPENCOG_TYPE_NODE_H

#include <opencog/atoms/proto/NameServer.h>
#include <opencog/atoms/base/Node.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 *
 * Experimental TypeNode class. This is a rough sketch for how things
 * like this might be done. It is not necessarily a good idea, and might
 * be replaced by something completely different, someday ...
 */

class TypeNode : public Node
{
protected:
	Type value;

public:
	// Please do NOT use this constructor!
	TypeNode(Type t, const std::string& s)
		// Convert to number and back to string to avoid miscompares.
		: Node(t, s),
		  value(nameserver().getType(s))
	{
		// Perform strict checking only for TypeNode.  The
		// DefinedTypeNode, which inherits from this class,
		// allows user-defined types which the classerver
		// currently does not know about.
		if (TYPE_NODE == t and NOTYPE == value)
			throw InvalidParamException(TRACE_INFO,
				"Not a valid typename: '%s'", s.c_str());
	}

public:
	TypeNode(const std::string& s)
		// Convert to number and back to string to avoid miscompares.
		: Node(TYPE_NODE, s),
		  value(nameserver().getType(s))
	{
		if (NOTYPE == value)
			throw InvalidParamException(TRACE_INFO,
				"Not a valid typename: '%s'", s.c_str());
	}

	TypeNode(Type t)
		: Node(TYPE_NODE, nameserver().getTypeName(t)),
		  value(t)
	{}

	TypeNode(Node &n)
		: Node(n),
		  value(nameserver().getType(n.get_name()))
	{
		OC_ASSERT(nameserver().isA(n.get_type(), TYPE_NODE),
			"Bad TypeNode constructor!");

		if (DEFINED_TYPE_NODE != _type and NOTYPE == value)
			throw InvalidParamException(TRACE_INFO,
				"Not a valid typename: '%s'", n.get_name().c_str());

		if (DEFINED_TYPE_NODE == _type and NOTYPE != value)
			throw InvalidParamException(TRACE_INFO,
				"Redefinition of a built-in typename: '%s'", n.get_name().c_str());
	}

	static void validate(const std::string& str)
	{
		Type t = nameserver().getType(str);
		// XXX TODO ... Some types are defined. In this case,
		// verify that the string occurs as a name inside
		// some DefineLink... if it does, then it's valid.
		// If it does not, then it's invalid.
		if (NOTYPE == t)
			throw InvalidParamException(TRACE_INFO,
				"Not a valid typename: '%s'", str.c_str());
	}

	Type get_value(void) { return value; }

	static Handle factory(const Handle&);
};

typedef std::shared_ptr<TypeNode> TypeNodePtr;
static inline TypeNodePtr TypeNodeCast(const Handle& h)
	{ return std::dynamic_pointer_cast<TypeNode>(h); }
static inline TypeNodePtr TypeNodeCast(AtomPtr a)
	{ return std::dynamic_pointer_cast<TypeNode>(a); }

#define createTypeNode std::make_shared<TypeNode>

/** @}*/
}

#endif // _OPENCOG_TYPE_NODE_H
