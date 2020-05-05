/*
 * BindLink.cc
 *
 * Copyright (C) 2009, 2014, 2015 Linas Vepstas
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

#include <opencog/util/oc_assert.h>
#include <opencog/atoms/atom_types/NameServer.h>
#include <opencog/atoms/core/UnorderedLink.h>
#include <opencog/atomspace/AtomSpace.h>

#include "BindLink.h"

using namespace opencog;

void BindLink::init(void)
{
	Type t = get_type();
	if (not nameserver().isA(t, BIND_LINK))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting a BindLink, got %s", tname.c_str());
	}

	_pat.redex_name = "anonymous BindLink";
}

BindLink::BindLink(const Handle& vardecl,
                   const Handle& body,
                   const Handle& rewrite)
	: BindLink(HandleSeq{vardecl, body, rewrite})
{}

BindLink::BindLink(const Handle& body, const Handle& rewrite)
	: BindLink(HandleSeq{body, rewrite})
{}

BindLink::BindLink(const HandleSeq&& hseq, Type t)
	: QueryLink(std::move(hseq), t)
{
	init();
}

/* ================================================================= */
/* ================================================================= */

/** Wrap query results in a SetLink, place them in the AtomSpace. */
ValuePtr BindLink::execute(AtomSpace* as, bool silent)
{
	QueueValuePtr qv(do_execute(as, silent));
	OC_ASSERT(qv->is_closed(), "Unexpected queue state!");
	HandleSeq rslt(qv->to_handle_seq());

	// If there is an anchor, then attach results to the anchor.
	// Otherwise, create a SetLink and return that.
	if (_variables._anchor and as)
	{
		for (const Handle& h: rslt)
			as->add_link(MEMBER_LINK, h, _variables._anchor);

		return _variables._anchor;
	}

	// The result_set contains a list of the grounded expressions.
	// (The order of the list has no significance, so it's really a set.)
	// Put the set into a SetLink, cache it, and return that.
	Handle rewr(createUnorderedLink(std::move(rslt), SET_LINK));

#define PLACE_RESULTS_IN_ATOMSPACE
#ifdef PLACE_RESULTS_IN_ATOMSPACE
	// Shoot. XXX FIXME. Most of the unit tests require that the atom
	// that we return is in the atomspace. But it would be nice if we
	// could defer this indefinitely, until its really needed.
	rewr = as->add_atom(rewr);
#endif /* PLACE_RESULTS_IN_ATOMSPACE */
	return rewr;
}

DEFINE_LINK_FACTORY(BindLink, BIND_LINK)

/* ===================== END OF FILE ===================== */
