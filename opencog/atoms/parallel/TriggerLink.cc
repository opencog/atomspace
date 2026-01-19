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
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <opencog/atoms/parallel/TriggerLink.h>
#include <opencog/util/exceptions.h>

using namespace opencog;

/// TriggerLink

TriggerLink::TriggerLink(const HandleSeq&& oset, Type t)
    : PureExecLink(std::move(oset), t)
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

void TriggerLink::setAtomSpace(AtomSpace* as)
{
	size_t sz = _outgoing.size();

	for (size_t i=0; i<sz-1; i++)
	{
		if (_outgoing[i]->is_executable())
			_outgoing[i]->execute(as, false);
	}

	ValuePtr vp(_outgoing[sz-1]);
	if (_outgoing[sz-1]->is_executable())
		vp = _outgoing[sz-1]->execute(as, false);

	throw ValueReturnException(vp);
}

DEFINE_LINK_FACTORY(TriggerLink, TRIGGER_LINK)
