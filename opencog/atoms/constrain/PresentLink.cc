/*
 * PresentLink.cc
 *
 * Copyright (C) 2017 Linas Vepstas
 *
 * Author: Linas Vepstas <linasvepstas@gmail.com>  January 2009, 2015, 2017
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

#include <opencog/atoms/free/FindUtils.h>
#include <opencog/atomspace/AtomSpace.h>
#include "PresentLink.h"

using namespace opencog;

void PresentLink::init(void)
{
	// The UnorderedLink ctor will have already sorted the outgoing set
	// for us into some order.  To find duplicates, we merely need to
	// iterate over the outgoing set, comparing neighbors.

	Arity sz = _outgoing.size();
	if (0 == sz) return;

	HandleSeq uniq;
	uniq.push_back(_outgoing[0]);  // The first one is always good.

	// Look for and remove duplicates.
	Arity lst = 0;
	Arity nxt = 1;
	while (nxt < sz)
	{
		if (_outgoing[lst] != _outgoing[nxt])
		{
			uniq.push_back(_outgoing[nxt]);
			lst = nxt;
		}
		nxt++;
	}

	// swap into place; faster than copy.
	_outgoing.swap(uniq);
}

PresentLink::PresentLink(const HandleSeq&& oset, Type t)
	: EvaluatableLink(std::move(oset), t)
{
	if (not nameserver().isA(t, PRESENT_LINK))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting an PresentLink, got %s", tname.c_str());
	}

	init();
}

/* ================================================================= */

/// The only way that a PresentLink or AbsentLink can be added an
/// AtomSpace is if they contain free variables. Adding constant terms
/// is flawed: If the constant wasn't there previously, it would get
/// added, and the test for presence would always succeed! And a test
/// for absence would always fail!
void PresentLink::setAtomSpace(AtomSpace* as)
{
	// Special-case ALWAYS_LINK and GROUP_LINK
	// This hack is here only until these two are eleminated
	// and replaced by strems (much like SortedStream but
	// grouping instead. We have this here to pass unit tests
	// until that time.)
	if (is_type(ALWAYS_LINK))
	{
		if (is_closed(get_handle()))
			throw SyntaxException(TRACE_INFO,
				"Cannot add a closed %s to the AtomSpace!  Got %s",
				nameserver().getTypeName(get_type()).c_str(),
				to_string().c_str());
		Link::setAtomSpace(as);
		return;
	}

	for (const Handle& h : _outgoing)
	{
		if (is_closed(h))
			throw SyntaxException(TRACE_INFO,
				"Cannot add a closed %s to the AtomSpace!  Got %s",
				nameserver().getTypeName(get_type()).c_str(),
				to_string().c_str());
	}

	Link::setAtomSpace(as);
}

/* ================================================================= */

/// Return true, if all of the outgoing set is present in the
/// indicated AtomSpace. It only makes sense to call this if
/// the current "this" pointer is not in any AtomSpace.
bool PresentLink::bevaluate(AtomSpace* as, bool silent)
{
	if (nullptr == as) return true;

	for (const Handle& h : _outgoing)
	{
		Handle maybe(as->get_atom(h));
		if (nullptr == maybe) return false;
	}

	return true;
}

// ---------------------------------------------------------------

DEFINE_LINK_FACTORY(PresentLink, PRESENT_LINK)

/* ===================== END OF FILE ===================== */
