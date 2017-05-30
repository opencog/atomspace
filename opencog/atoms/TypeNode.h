/*
 * opencog/atoms/TypeNode.h
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

#include <opencog/atoms/base/ClassServer.h>
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
	TypeNode(const std::string& s)
		// Convert to number and back to string to avoid miscompares.
		: Node(TYPE_NODE, s),
		  value(classserver().getType(s))
	{
		if (NOTYPE == value)
			throw InvalidParamException(TRACE_INFO,
				"Not a valid typename: '%s'", s.c_str());
	}

	TypeNode(Type t)
		: Node(TYPE_NODE, classserver().getTypeName(t)),
		  value(t)
	{}

	TypeNode(Node &n)
		: Node(n),
		  value(classserver().getType(n.getName()))
	{
		OC_ASSERT(classserver().isA(n.getType(), TYPE_NODE),
			"Bad TypeNode constructor!");

		if (DEFINED_TYPE_NODE != _type and NOTYPE == value)
			throw InvalidParamException(TRACE_INFO,
				"Not a valid typename: '%s'", n.getName().c_str());

		if (DEFINED_TYPE_NODE == _type and NOTYPE != value)
			throw InvalidParamException(TRACE_INFO,
				"Redefinition of a built-in typename: '%s'", n.getName().c_str());
	}

	static void validate(const std::string& str)
	{
		Type t = classserver().getType(str);
		// XXX TODO ... Some types are defined. In this case,
		// verify that the string occurs as a name inside
		// some DefineLink... if it does, then it's valid.
		// If it does not, then it's invalid.
		if (NOTYPE == t)
			throw InvalidParamException(TRACE_INFO,
				"Not a valid typename: '%s'", str.c_str());
	}

	Type get_value(void) { return value; }
};

typedef std::shared_ptr<TypeNode> TypeNodePtr;
static inline TypeNodePtr TypeNodeCast(const Handle& h)
	{ AtomPtr a(h); return std::dynamic_pointer_cast<TypeNode>(a); }
static inline TypeNodePtr TypeNodeCast(AtomPtr a)
	{ return std::dynamic_pointer_cast<TypeNode>(a); }

// XXX temporary hack ...
#define createTypeNode std::make_shared<TypeNode>

/** @}*/
}

#endif // _OPENCOG_TYPE_NODE_H
