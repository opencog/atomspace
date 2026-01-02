/*
 * PipeLink.cc
 *
 * Copyright (C) 2015 Linas Vepstas
 * Copyright (C) 2026 BrainyBlaze Dynamics, LLC
 *
 * Author: Linas Vepstas <linasvepstas@gmail.com>  May 2015
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the
 * exceptions at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 */

#include "PipeLink.h"

using namespace opencog;

void PipeLink::init(void)
{
	if (_type != PIPE_LINK) return;

	// Must have name and body
	if (2 != _outgoing.size())
		throw SyntaxException(TRACE_INFO,
			"Expecting name and data stream, got %s", to_string().c_str());

	// Type-check. For now, we can only bind NameNodes.
	Type ntype = _outgoing[0]->get_type();
	if (NAME_NODE != ntype)
		throw SyntaxException(TRACE_INFO,
			"Expecting NameNode, got %s", to_string().c_str());

	// Perform some additional checks in the UniqueLink init method
	UniqueLink::init();
}

PipeLink::PipeLink(const HandleSeq&& oset, Type t)
	: UniqueLink(std::move(oset), t)
{
	init();
}

/**
 * Get the stream associated with the name.
 * This will be the second atom of some PipeLink, where
 * `name` is the first.
 */
Handle PipeLink::get_stream(const Handle& name, const AtomSpace* as)
{
	Handle uniq(get_unique(name, PIPE_LINK, false, as));
	return uniq->getOutgoingAtom(1);
}

Handle PipeLink::get_link(const Handle& name, const AtomSpace* as)
{
	return get_unique(name, PIPE_LINK, false, as);
}

DEFINE_LINK_FACTORY(PipeLink, PIPE_LINK)

/* ===================== END OF FILE ===================== */
