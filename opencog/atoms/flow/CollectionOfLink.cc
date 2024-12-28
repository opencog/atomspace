/*
 * CollectionOfLink.cc
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

	_out_type = SET_LINK;
	_out_is_link = true;
	_have_typespec = false;

	check_typespec();
}

// ---------------------------------------------------------------

/// Check for valid form
void CollectionOfLink::check_typespec(void)
{
	if (1 == _outgoing.size())
	{
		_have_typespec = false;
		return;
	}

	// If there are two args, then the first one specifies the
	// output type.
	if (2 != _outgoing.size())
		throw InvalidParamException(TRACE_INFO,
			"Expecting one or two args, got %s",
				to_string().c_str());

	_have_typespec = true;

	// FIXME: _outoging[0] could be executable, in which case
	// is should be executed, first. But I'm lazy. Also:
	// instead of being a simple type, the output could be
	// a complicated signature. Again, I'm lazy.
	if (not _outgoing[0]->is_type(TYPE_NODE))
		throw InvalidParamException(TRACE_INFO,
			"Expecting first arg to be a type, got %s",
				to_string().c_str());
	_out_type = TypeNodeCast(_outgoing[0])->get_kind();

	_out_is_link = nameserver().isLink(_out_type);
	if (not _out_is_link and not nameserver().isA(_out_type, LINK_VALUE))
		throw InvalidParamException(TRACE_INFO,
			"Expecting type to be a Link or LinkValue, got %s for %s",
				nameserver().getTypeName(_out_type).c_str(),
				to_string().c_str());
}

// ---------------------------------------------------------------

ValuePtr CollectionOfLink::rewrap_h(AtomSpace* as, const Handle& base)
{
	if (base->is_node())
	{
		if (_out_is_link)
			return as->add_link(_out_type, base);
		return createLinkValue(_out_type, ValueSeq({base}));
	}

	if (_out_is_link)
		return as->add_link(_out_type,
			HandleSeq(base->getOutgoingSet()));

	return createLinkValue(_out_type, base->getOutgoingSet());
}

// ---------------------------------------------------------------

ValuePtr CollectionOfLink::rewrap_v(AtomSpace* as, const ValuePtr& vp)
{
	if (not vp->is_type(LINK_VALUE))
		throw InvalidParamException(TRACE_INFO,
			"CollectionOfLink expects a LinkValue, got %s",
			vp->to_string().c_str());

	LinkValuePtr lvp(LinkValueCast(vp));

	if (_out_is_link)
	{
		HandleSeq hs = lvp->to_handle_seq();
		return as->add_link(_out_type, std::move(hs));
	}

	if (vp->get_type() == _out_type)
		return vp;

	return createLinkValue(_out_type, lvp->value());
}

// ---------------------------------------------------------------

/// Return a SetLink vector.
ValuePtr CollectionOfLink::execute(AtomSpace* as, bool silent)
{
	int coff = 0;
	if (_have_typespec) coff = 1;

	// If the atom is not executable, then re-wrap it, as appropriate.
	Handle base(_outgoing[coff]);
	if (not base->is_executable())
		return rewrap_h(as, base);

	// If the given Atom is executable, then execute it, and
	// branch over all the various possibilities.
	ValuePtr vp = base->execute(as, silent);

	// Hmmm. FilterLink returns null pointer when the filter
	// is empty. Is this a bug in FilterLink? Not sure any more.
	if (nullptr == vp)
	{
		if (_out_is_link)
			return createLink(_out_type);
		return createLinkValue(_out_type, ValueSeq({}));
	}

	if (vp->is_atom())
		return rewrap_h(as, HandleCast(vp));

	return rewrap_v(as, vp);
}

DEFINE_LINK_FACTORY(CollectionOfLink, COLLECTION_OF_LINK)

/* ===================== END OF FILE ===================== */
