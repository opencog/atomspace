/*
 * CollectionOfLink.cc
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

#include "CollectionOfLink.h"

using namespace opencog;

CollectionOfLink::CollectionOfLink(const HandleSeq&& oset, Type t)
	: FunctionLink(std::move(oset), t)
{
	if (not nameserver().isA(t, COLLECTION_OF_LINK))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting an CollectionOfLink, got %s", tname.c_str());
	}

	if (1 != _outgoing.size() and 2 != _outgoing.size())
		throw InvalidParamException(TRACE_INFO,
			"CollectionOfLink expects one or two args, got %s",
				to_string().c_str());
}

// ---------------------------------------------------------------

/// Return a SetLink vector.
ValuePtr CollectionOfLink::execute(AtomSpace* as, bool silent)
{
	int coff = 0;
	Type otype = SET_LINK;
	bool type_is_link = true;

	// If there are two args, then the first one specifies the
	// output type.
	if (2 == _outgoing.size())
	{
		coff = 1;

		// FIXME: _outoging[0] could be executable, in which case
		// is should be executed, first. But I'm lazy. Also:
		// instead of being a simple type, the output could be
		// a complicated signature. Again, I'm lazy.
		if (not _outgoing[0]->is_type(TYPE_NODE))
			throw InvalidParamException(TRACE_INFO,
				"Expecting first arg of a CollectionOfLink to be a type, got %s",
					to_string().c_str());
		otype = TypeNodeCast(_outgoing[0])->get_kind();

		type_is_link = nameserver().isLink(otype);
		if (not type_is_link and not nameserver().isA(otype, LINK_VALUE))
			throw InvalidParamException(TRACE_INFO,
				"Expecting type of a CollectionOfLink to be a Link or LinkValue %s",
					nameserver().getTypeName(otype).c_str());
	}

	// If the atom is not executable, then re-wrap it, as appropriate.
	Handle base(_outgoing[coff]);
	if (not base->is_executable())
	{
		if (type_is_link)
			return as->add_link(otype, base);
		else
			return createLinkValue(otype, ValueSeq({base}));
	}

	// If the given Atom is executable, then execute it.
	// In effectively all cases, we expect it to be executable!
	// How we re-wrap it depends on the execution output.
	ValuePtr vp = base->execute(as, silent);
	if (vp->is_node())
	{
		if (type_is_link)
			return as->add_link(otype, HandleCast(vp));
		else
			return createLinkValue(otype, ValueSeq({vp}));
	}

	if (vp->is_link())
	{
		if (type_is_link)
			return as->add_link(otype,
				HandleSeq(HandleCast(vp)->getOutgoingSet()));
		else
		{
			ValueSeq vs;
			for (const Handle& h : HandleCast(vp)->getOutgoingSet())
				vs.push_back(h);
			return createLinkValue(otype, std::move(vs));
		}
	}

	if (not vp->is_type(LINK_VALUE))
		throw InvalidParamException(TRACE_INFO,
			"CollectionOfLink expects a LinkValue, got %s",
			vp->to_string().c_str());

	LinkValuePtr lvp = LinkValueCast(vp);

	if (type_is_link)
	{
		HandleSeq hs = lvp->to_handle_seq();
		return as->add_link(otype, std::move(hs));
	}

	if (vp->get_type() == otype)
		return vp;

	return createLinkValue(otype, lvp->value());
}

DEFINE_LINK_FACTORY(CollectionOfLink, COLLECTION_OF_LINK)

/* ===================== END OF FILE ===================== */
