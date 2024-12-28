/*
 * ConcatenateLink.cc
 *
 * Copyright (C) 2015, 2022, 2024 Linas Vepstas
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

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/value/LinkValue.h>

#include "ConcatenateLink.h"

using namespace opencog;

ConcatenateLink::ConcatenateLink(const HandleSeq&& oset, Type t)
	: CollectionOfLink(std::move(oset), t)
{
	if (not nameserver().isA(t, CONCATENATE_LINK))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting an ConcatenateLink, got %s", tname.c_str());
	}

	if (not _have_typespec)
	{
		_out_type = LINK_VALUE;
		_out_is_link = false;
	}
}

// ---------------------------------------------------------------

/// Return a concatenation of lists.
ValuePtr ConcatenateLink::rewrap_h(AtomSpace* as, const Handle& base)
{
	// We expect a link. We can throw, or we can be silent!?
	// I dunno. Flip a coin.
	if (not base->is_link()) return base;
	HandleSeq oset;
	for (const Handle& oli : base->getOutgoingSet())
	{
		if (oli->is_link())
		{
			const HandleSeq& los = oli->getOutgoingSet();
			oset.insert(oset.end(), los.begin(), los.end());
		}
		else
			oset.push_back(oli);
	}
	if (not _have_typespec) _out_type = base->get_type();
	return as->add_link(_out_type, std::move(oset));
}

// ---------------------------------------------------------------

/// Return a concatenation of lists.
ValuePtr ConcatenateLink::rewrap_v(AtomSpace* as, const ValuePtr& vp)
{
	if (not vp->is_type(LINK_VALUE))
		throw InvalidParamException(TRACE_INFO,
			"ConcatenateLink expects a LinkValue, got %s",
			vp->to_string().c_str());

	ValueSeq vseq;
	for (const ValuePtr& vli : LinkValueCast(vp)->value())
	{
		if (vli->is_type(LINK_VALUE))
		{
			const ValueSeq& vos = LinkValueCast(vli)->value();
			vseq.insert(vseq.end(), vos.begin(), vos.end());
		}
		else
			vseq.push_back(vli);
	}
	return createLinkValue(_out_type, std::move(vseq));
}

// ---------------------------------------------------------------

DEFINE_LINK_FACTORY(ConcatenateLink, CONCATENATE_LINK)

/* ===================== END OF FILE ===================== */
