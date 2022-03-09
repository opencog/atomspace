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

#include <opencog/util/oc_assert.h>
#include <opencog/atoms/atom_types/NameServer.h>
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
	Type _kind;

public:
	// Please do NOT use this constructor!
	TypeNode(Type t, const std::string&& s)
		// Convert to number and back to string to avoid miscompares.
		: Node(t, std::move(s)),
		  _kind(nameserver().getType(_name))
	{
		// Perform strict checking only for TypeNode.  The
		// DefinedTypeNode, which inherits from this class,
		// allows user-defined types which the classerver
		// currently does not know about.
		if (TYPE_NODE == t and NOTYPE == _kind)
			throw InvalidParamException(TRACE_INFO,
				"Not a valid typename: '%s'", _name.c_str());
	}

public:
	TypeNode(const std::string&& s)
		// Convert to number and back to string to avoid miscompares.
		: Node(TYPE_NODE, std::move(s)),
		  _kind(nameserver().getType(_name))
	{
		if (NOTYPE == _kind)
			throw InvalidParamException(TRACE_INFO,
				"Not a valid typename: '%s'", s.c_str());
	}

	TypeNode(Type t)
		: Node(TYPE_NODE, std::string(nameserver().getTypeName(t))),
		  _kind(t)
	{}

	TypeNode(TypeNode&) = delete;
	TypeNode& operator=(const TypeNode&) = delete;

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

	Type get_kind(void) const { return _kind; }

	static Handle factory(const Handle&);
};

NODE_PTR_DECL(TypeNode)
#define createTypeNode CREATE_DECL(TypeNode)

/** @}*/
}

#endif // _OPENCOG_TYPE_NODE_H
