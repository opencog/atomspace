/*
 * opencog/atoms/parallel/NameNode.cc
 *
 * Copyright (C) 2009, 2013-2015, 2020, 2024, 2025 Linas Vepstas
 * Copyright (C) 2026 BrainyBlaze Dynamics, LLC
 * SPDX-License-Identifier: AGPL-3.0-or-later
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
 */

#include <opencog/atoms/flow/NameNode.h>
// #include <opencog/atoms/grant/PipeLink.h>
#include <opencog/atomspace/AtomSpace.h>

using namespace opencog;

NameNode::NameNode(const std::string&& str) :
	Node(NAME_NODE, std::move(str))
{
}

NameNode::NameNode(Type t, const std::string&& str) :
	Node(t, std::move(str))
{
	if (not is_type(NAME_NODE))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting a NameNode, got %s", tname.c_str());
	}
}

ValuePtr NameNode::execute(AtomSpace* as, bool silent)
{
#if 0
	Handle defn(DefineLink::get_definition(get_handle()));
	if (nullptr == defn)
		throw RuntimeException(TRACE_INFO,
			"NameNode \"%s\" is not defined", get_name().c_str());

	if (not defn->is_executable()) return defn;

	ValuePtr val = defn->execute();
	return val;
#endif
return nullptr;
}

DEFINE_NODE_FACTORY(NameNode, NAME_NODE)
