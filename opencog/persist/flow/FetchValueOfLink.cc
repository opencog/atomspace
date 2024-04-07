/*
 * FetchValueOfLink.cc
 *
 * Copyright (C) 2015, 2022 Linas Vepstas
 *
 * Author: Linas Vepstas <linasvepstas@gmail.com>  January 2009
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
 *
 * You should have received a copy of the GNU Affero General Public
 * License along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <opencog/persist/api/StorageNode.h>

#include "FetchValueOfLink.h"

using namespace opencog;

FetchValueOfLink::FetchValueOfLink(const HandleSeq&& oset, Type t)
	: ValueOfLink(std::move(oset), t)
{
	if (not nameserver().isA(t, FETCH_VALUE_OF_LINK))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting an FetchValueOfLink, got %s", tname.c_str());
	}
	init();
}

void FetchValueOfLink::init(void)
{
	size_t ary = _outgoing.size();

	if (3 != ary)
		throw SyntaxException(TRACE_INFO, "Expecting three atoms!");

	if (not _outgoing[2]->is_type(STORAGE_NODE))
		throw SyntaxException(TRACE_INFO, "Expecting a StorageNode, got %s",
			_outgoing[2]->to_string().c_str());
}

// ---------------------------------------------------------------

/// Fetch the Value first, and then return it.
ValuePtr FetchValueOfLink::execute(AtomSpace* as, bool silent)
{
	StorageNodePtr stnp = StorageNodeCast(_outgoing[2]);

	// XXX TODO FIXME ... if either of _outgoing[0] or _outgoing[1]
	// are executable, then they need to be executed, first, right?
	// Because that's the usual intent. Else they'd be wrapped in a
	// DontExecLink, right? I'm confused.

	// If the StorageNode is not open for reading, it will
	// either throw, or do something else. Not our decision.
	Handle ah(as->add_atom(_outgoing[0]));
	Handle ak(as->add_atom(_outgoing[1]));
	stnp->fetch_value(ah, ak, as);
	stnp->barrier();

	return ah->getValue(ak);
}

DEFINE_LINK_FACTORY(FetchValueOfLink, FETCH_VALUE_OF_LINK)

void opencog_persist_flow_init(void)
{
	// Force shared lib ctors to run
};


/* ===================== END OF FILE ===================== */
