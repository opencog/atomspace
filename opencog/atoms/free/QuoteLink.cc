/*
 * QuoteLink.cc
 *
 * Copyright (C) 2025 OpenCog Foundation
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

#include "QuoteLink.h"
#include "Quotation.h"

using namespace opencog;

QuoteLink::QuoteLink(const HandleSeq&& oset, Type t)
	: Link(std::move(oset), t)
{
	// Type must be as expected
	if (not nameserver().isA(t, QUOTE_LINK))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting a QuoteLink, got %s", tname.c_str());
	}
}

// ---------------------------------------------------------------

/// Helper: recursively walk tree, consuming UnquoteLinks at level 1.
/// This implements the paired Quote/Unquote semantics.
/// Quote and Unquote are paired brackets: (Quote (stuff (Unquote X)))
/// when unwrapped becomes (stuff X).
static Handle consume_unquotes(const Handle& h, Quotation quotation)
{
	Type t = h->get_type();

	// At level 1, UnquoteLinks are consumed (their children inlined)
	if (quotation.level() == 1 and UNQUOTE_LINK == t)
	{
		// UnquoteLink with single child: return child
		if (1 == h->get_arity())
			return h->getOutgoingAtom(0);

		// UnquoteLink with multiple children: will be inlined by parent
		// Return as-is, parent will handle the inlining
		return h;
	}

	// Nested QuoteLink: don't descend further (it's a new quote context)
	// Just return as-is - it will handle its own unquotes when executed
	if (QUOTE_LINK == t)
		return h;

	// Node: nothing to do
	if (h->is_node())
		return h;

	// Link: recurse into children
	quotation.update(t);
	HandleSeq new_oset;
	bool changed = false;

	for (const Handle& child : h->getOutgoingSet())
	{
		// Check if child is an UnquoteLink at level 1 with multiple children
		// These need to be inlined (spread into parent)
		if (quotation.level() == 1 and
		    UNQUOTE_LINK == child->get_type() and
		    child->get_arity() > 1)
		{
			// Inline all children of the UnquoteLink
			for (const Handle& uc : child->getOutgoingSet())
				new_oset.emplace_back(uc);
			changed = true;
		}
		else
		{
			Handle new_child = consume_unquotes(child, quotation);
			if (new_child != child)
				changed = true;
			new_oset.emplace_back(new_child);
		}
	}

	if (not changed)
		return h;

	return createLink(std::move(new_oset), t);
}

// ---------------------------------------------------------------

/// Execute the QuoteLink by consuming it and its paired UnquoteLinks.
/// Quote and Unquote are paired brackets:
/// (Quote (stuff (Unquote X))) -> (stuff X)
ValuePtr QuoteLink::execute(AtomSpace* as, bool silent)
{
	// Handle empty QuoteLink
	if (0 == _outgoing.size())
		return Handle::UNDEFINED;

	// Single argument case: unwrap it
	if (1 == _outgoing.size())
	{
		Handle content = _outgoing[0];

		// Direct involution: (Quote (Unquote X)) -> X
		if (content->get_type() == UNQUOTE_LINK and
		    1 == content->get_arity())
		{
			Handle result = content->getOutgoingAtom(0);
			if (as) return as->add_atom(result);
			return result;
		}

		// Walk tree and consume level-1 UnquoteLinks
		Quotation quotation(1);  // Start at level 1 (inside the Quote)
		Handle result = consume_unquotes(content, quotation);
		if (as) return as->add_atom(result);
		return result;
	}

	// Multiple arguments case: return self (do nothing)
	return get_handle();
}

DEFINE_LINK_FACTORY(QuoteLink, QUOTE_LINK)

/* ===================== END OF FILE ===================== */
