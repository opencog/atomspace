/*
 * LinkSignatureLink.cc
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

#include <opencog/atoms/core/TypeNode.h>
#include <opencog/atoms/value/LinkValue.h>

#include "LinkSignatureLink.h"

using namespace opencog;

LinkSignatureLink::LinkSignatureLink(const HandleSeq&& oset, Type t)
	: Link(std::move(oset), t)
{
	if (not nameserver().isA(t, LINK_SIGNATURE_LINK))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting an LinkSignatureLink, got %s", tname.c_str());
	}

	if (oset.size() < 1)
		throw InvalidParamException(TRACE_INFO,
			"Expecting LinkSignatureLink with at least one argument");

	if (TYPE_NODE != oset[0]->get_type())
		throw InvalidParamException(TRACE_INFO,
			"LinkSignatureLink only supports TypeNode at this time, got %s",
			oset[0]->to_string().c_str());

	_kind = TypeNodeCast(oset[0])->get_kind();
}

// ---------------------------------------------------------------

/// Return either a Link or a LinkValue of the desired type.
ValuePtr LinkSignatureLink::execute(AtomSpace* as, bool silent)
{
	HandleSeq noset;
	for (size_t i=1; i < _outgoing.size(); i++)
		noset.emplace_back(_outgoing[i]);

	if (LINK_VALUE == _kind)
		return createLinkValue(noset);

	if (nameserver().isA(_kind, LINK))
		return createLink(noset, _kind);

	// Should support other kinds too.
	const std::string& tname = nameserver().getTypeName(_kind);
	throw InvalidParamException(TRACE_INFO,
		"Unsupported type %s", tname.c_str());
}

DEFINE_LINK_FACTORY(LinkSignatureLink, LINK_SIGNATURE_LINK)

/* ===================== END OF FILE ===================== */
