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

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/core/TypeNode.h>
#include <opencog/atoms/value/LinkValue.h>
#include <opencog/atoms/value/ValueFactory.h>

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

	if (not oset[0]->is_type(TYPE_NODE))
		throw InvalidParamException(TRACE_INFO,
			"LinkSignatureLink only supports TypeNode at this time, got %s",
			oset[0]->to_string().c_str());

	_kind = TypeNodeCast(oset[0])->get_kind();
}

// ---------------------------------------------------------------

/// Return a LinkValue of the desired type.
/// If kind is a Link type, then return that Link.
/// For this case, if anything in the newset is NOT an Atom,
/// it is silently ignored. (XXX FIXME Perhaps exception should
/// be thrown? I dunno.)
ValuePtr LinkSignatureLink::construct(AtomSpace* as, const ValueSeq&& newset) const
{
	if (nameserver().isA(_kind, LINK_VALUE))
		return valueserver().create(_kind, std::move(newset));

	if (nameserver().isA(_kind, LINK))
	{
		HandleSeq oset;
		for (const ValuePtr& vp : newset)
		{
			const Handle& h(HandleCast(vp));
			if (h) oset.push_back(h);
		}
		return as->add_link(_kind, std::move(oset));
	}

	// Should support other kinds too.  XXX FIXME
	// (???) I guess we could also cast FloatVectors to NumberNodes
	// or perform other kinds of transformations between vectors
	// and LinkValues, ... or something. Unclear at this time.
	const std::string& tname = nameserver().getTypeName(_kind);
	throw InvalidParamException(TRACE_INFO,
		"Unsupported type %s", tname.c_str());
}

// ---------------------------------------------------------------

/// Return either a Link or a LinkValue of the desired type.
ValuePtr LinkSignatureLink::execute(AtomSpace* as, bool silent)
{
	// The _kind will usually be some LinkValue. One interesting
	// case is the stream, which takes some Handle argument that
	// controls the stream operation. Examples include SortedStream
	// and FlatStream. Pss that directly to the correct factory.
	if (nameserver().isA(_kind, HANDLE_ARG))
		return valueserver().create(_kind, _outgoing[1]);

	ValueSeq voset;
	for (size_t i=1; i < _outgoing.size(); i++)
	{
		// As always, propagate execution down to leaves.
		// FYI, some of these may be ValueShimLinks. (Yuck)
		if (_outgoing[i]->is_executable())
			voset.emplace_back(_outgoing[i]->execute(as, silent));
		else
			voset.emplace_back(_outgoing[i]);
	}

	return construct(as, std::move(voset));
}

DEFINE_LINK_FACTORY(LinkSignatureLink, LINK_SIGNATURE_LINK)

/* ===================== END OF FILE ===================== */
