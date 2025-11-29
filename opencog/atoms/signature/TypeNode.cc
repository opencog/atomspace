/*
 * opencog/atoms/core/TypeNode.cc
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

#include "TypeNode.h"

using namespace opencog;

/// Return true, if Type t is of the kind specified in this TypeNode.
bool TypeNode::is_kind(Type t) const
{
	if (TYPE_NODE == _type)
		return t == _kind;

	if (TYPE_INH_NODE == _type)
		return nameserver().isA(t, _kind);

	if (TYPE_CO_INH_NODE == _type)
		return nameserver().isA(_kind, t);

	throw RuntimeException(TRACE_INFO,
		"Support for %s not implemented!\n",
		nameserver().getTypeName(_type).c_str());

	return false;
}

DEFINE_NODE_FACTORY(TypeNode, TYPE_NODE)
