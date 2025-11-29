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
#include <opencog/atoms/core/NumberNode.h>
#include <opencog/atoms/signature/TypeNode.h>
#include <opencog/atoms/value/LinkValue.h>
#include <opencog/atoms/value/StringValue.h>
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

	// XXX FIXME: we could allow complex types here, e.g. SignatureLink
	// or whatever, as long as that signature has only one blank slot
	// in it. If there's more than one blank, then ... well, see the
	// wiki page https://wiki.opencog.org/w/LinkSignatureLink for what
	// alse could be done here, and a hint of maybe why it should not be.
	if (not oset[0]->is_type(TYPE_NODE))
		throw InvalidParamException(TRACE_INFO,
			"LinkSignatureLink only supports TypeNode at this time, got %s",
			oset[0]->to_string().c_str());

	_kind = TypeNodeCast(oset[0])->get_kind();
}

// ---------------------------------------------------------------

/// Rewrite the provided input into the desired type. This will rewrite
/// between Links and LinkValues, going in eitehr direction, and between
/// Nodes and StringValues, going in either direction, and between
/// NumberNodes and FloatValues, going in eitehr direction.
//
ValuePtr LinkSignatureLink::do_construct(const ValueSeq&& newset) const
{
	if (nameserver().isA(_kind, LINK_VALUE))
		return valueserver().create(_kind, std::move(newset));

	if (nameserver().isA(_kind, LINK))
	{
		HandleSeq oset;
		for (const ValuePtr& vp : newset)
		{
			if (not vp->is_atom())
				throw RuntimeException(TRACE_INFO,
					"Expecting Atom, got %s\n", vp->to_string().c_str());

			oset.emplace_back(HandleCast(vp));
		}
		return createLink(std::move(oset), _kind);
	}

	if (nameserver().isA(_kind, NUMBER_NODE) or
	    nameserver().isA(_kind, FLOAT_VALUE))
	{
		std::vector<double> numvec;
		for (const ValuePtr& vp : newset)
		{
			if (vp->is_type(NUMBER_NODE))
			{
				const std::vector<double>& nums(NumberNodeCast(vp)->value());
				numvec.insert(numvec.end(), nums.begin(), nums.end());
				continue;
			}
			if (vp->is_type(FLOAT_VALUE))
			{
				const std::vector<double>& nums(FloatValueCast(vp)->value());
				numvec.insert(numvec.end(), nums.begin(), nums.end());
				continue;
			}
			throw RuntimeException(TRACE_INFO,
				"Expecting Atom, got %s\n", vp->to_string().c_str());
		}

		if (NUMBER_NODE == _kind)
			return createNumberNode(std::move(numvec));

		return valueserver().create(_kind, std::move(numvec));
	}

	if (nameserver().isA(_kind, NODE) or
	    nameserver().isA(_kind, STRING_VALUE))
	{
		if (1 != newset.size())
			throw RuntimeException(TRACE_INFO,
				"Expecting Node or String Value of size one, got size %lu\n",
				newset.size());

		std::string name;
		if (newset[0]->is_type(NODE))
			name = NodeCast(newset[0])->get_name();

		else if (newset[0]->is_type(STRING_VALUE))
		{
			if (0 == newset[0]->size())
				throw RuntimeException(TRACE_INFO,
					"Expecting non-empty StringValue\n");

			name = StringValueCast(newset[0])->value()[0];
		}
		else
			throw RuntimeException(TRACE_INFO,
				"Expecting source to be Node or StringValue, got %s\n",
				newset[0]->to_string().c_str());

		if (nameserver().isA(_kind, NODE))
			return createNode(_kind, std::move(name));

		return valueserver().create(_kind, std::move(name));
	}

	// Should support other kinds too.  XXX FIXME
	// (???) Are there any other kinds of transformations
	// between vectors and LinkValues that would be natural?
	const std::string& tname = nameserver().getTypeName(_kind);
	throw InvalidParamException(TRACE_INFO,
		"Unsupported type %s", tname.c_str());
}

ValuePtr LinkSignatureLink::construct(AtomSpace* as, const ValueSeq&& newset) const
{
	// The LinkSignature ise used to rewrite Values ... which means those
	// Values will typically NOT be resident in the AtomSpace, but will
	// be in some ValueShimLink. During such rewrites (coming from
	// Replacement::substitute_scoped()) the AtomSpace ptr will be
	// a nullptr, which is OK, because the ValueShim could not have
	// been inserted into it anyway. So .. check for nullptr==as.
	ValuePtr vp(do_construct(std::move(newset)));
	if (as and vp->is_atom())
		return as->add_atom(HandleCast(vp));
	return vp;
}

// ---------------------------------------------------------------

/// Return either a Link or a LinkValue of the desired type.
ValuePtr LinkSignatureLink::execute(AtomSpace* as, bool silent)
{
	// The _kind will usually be some LinkValue. One interesting
	// case is the stream, which takes some Handle argument that
	// controls the stream operation. Examples include SortedStream
	// and FlatStream. Pss that directly to the correct factory.
	if (nameserver().isA(_kind, HANDLE_ARG) and 2 == _outgoing.size())
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
