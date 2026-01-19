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
#include <opencog/atoms/grant/PipeLink.h>
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
	// Pipe definitions might be located in an AtomSpace that is
	// a child of the AtomSpace holding this NameNode.
	const AtomSpace* search_as = as ? as : getAtomSpace();
	Handle strm(PipeLink::get_stream(get_handle(), search_as));

	// It seems we have a choice of two implementations, here. We can
	// complain that the desired does not yet have a stream associated
	// with it, or we can block and wait, until some other thread
	// provides a definition.  Both implementations seem plausible
	// The first is easier to debug when Atomese is hand-written;
	// the second seems more appropriate for automation.
	if (nullptr == strm)
		throw RuntimeException(TRACE_INFO,
			"Not yet defined: %s", to_string().c_str());

	// The call to execute() needs to be "passed through" to the
	// stream, so that executing NameNode behaves exactly the same
	// way as executing the stream that is named.
	if (not strm->is_executable())
		return strm;
	return strm->execute(as, silent);
}

DEFINE_NODE_FACTORY(NameNode, NAME_NODE)
