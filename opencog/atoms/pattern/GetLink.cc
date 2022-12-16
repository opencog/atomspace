/*
 * GetLink.cc
 *
 * Copyright (C) 2019 Linas Vepstas
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

#include <opencog/util/oc_assert.h>
#include <opencog/atoms/atom_types/NameServer.h>
#include <opencog/atoms/core/UnorderedLink.h>
#include <opencog/query/Satisfier.h>

#include "GetLink.h"

using namespace opencog;

void GetLink::init(void)
{
	Type t = get_type();
	if (not nameserver().isA(t, GET_LINK))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting a GetLink, got %s", tname.c_str());
	}
}

GetLink::GetLink(const HandleSeq&& hseq, Type t)
	: MeetLink(std::move(hseq), t)
{
	init();
}

/* ================================================================= */

ValuePtr GetLink::execute(AtomSpace* as, bool silent)
{
	QueueValuePtr qv(MeetLink::do_execute(as, silent));
	OC_ASSERT(qv->is_closed(), "Unexpected queue state!");
	HandleSet hs(qv->to_handle_set());

	// Create the satisfying set, and cache it.
	Handle satset(createUnorderedLink(std::move(hs), SET_LINK));

#define PLACE_RESULTS_IN_ATOMSPACE
#ifdef PLACE_RESULTS_IN_ATOMSPACE
	// Shoot. XXX FIXME. Most of the unit tests require that the atom
	// that we return is in the atomspace. But it would be nice if we
	// could defer this indefinitely, until its really needed.
	if (as) satset = as->add_atom(satset);
#endif /* PLACE_RESULTS_IN_ATOMSPACE */

	return satset;
}

DEFINE_LINK_FACTORY(GetLink, GET_LINK)

/* ===================== END OF FILE ===================== */
