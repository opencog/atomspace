/*
 * opencog/atoms/parallel/TriggerLink.cc
 *
 * Copyright (C) 2009, 2013-2015, 2020, 2024, 2025 Linas Vepstas
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

#include <opencog/atoms/parallel/TriggerLink.h>
#include <opencog/util/exceptions.h>

using namespace opencog;

/// TriggerLink

TriggerLink::TriggerLink(const HandleSeq&& oset, Type t)
    : Link(std::move(oset), t)
{
	if (0 == oset.size())
		throw SyntaxException(TRACE_INFO,
			"TriggerLink expects at least one Atom!");
}

void TriggerLink::install()
{
	throw SyntaxException(TRACE_INFO,
		"TriggerLinks cannot be placed into other Links!");
}

ValuePtr TriggerLink::execute(AtomSpace* as, bool silent)
{
	size_t sz = _outgoing.size();

	// I dunno. As I am writing this, this seems like an OK idea
	// to run all of them, but this is just ... also ... pointless?
	// Maybe stupid? Bad idea? I don't know...
	for (size_t i=0; i<sz-1; i++)
	{
		if (_outgoing[i]->is_executable())
			_outgoing[i]->execute(as, silent);
	}

	if (_outgoing[sz-1]->is_executable())
		return _outgoing[sz-1]->execute(as, silent);

	return _outgoing[sz-1];
}

void TriggerLink::setAtomSpace(AtomSpace* as)
{
	throw ValueReturnException(execute(as, false));
}

DEFINE_LINK_FACTORY(TriggerLink, TRIGGER_LINK)
