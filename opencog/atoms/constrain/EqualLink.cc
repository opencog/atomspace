/*
 * opencog/atoms/constrain/EqualLink.cc
 *
 * Copyright (C) 2025 BrainyBlaze Dynamics, LLC
 * All Rights Reserved
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <opencog/atoms/atom_types/atom_types.h>
#include <opencog/atoms/base/ClassServer.h>
#include <opencog/atoms/free/FindUtils.h>
#include <opencog/atoms/value/BoolValue.h>
#include <opencog/atoms/value/LinkValue.h>
#include <opencog/atomspace/AtomSpace.h>
#include "EqualLink.h"

using namespace opencog;

EqualLink::EqualLink(const HandleSeq&& oset, Type t)
    : EvaluatableLink(std::move(oset), t)
{
	if (not nameserver().isA(t, EQUAL_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting an EqualLink");
}

/* ================================================================= */

/// The only EqualLinks allowed into the AtomSpace are those that
/// contain exectuable Links (which will be executed at runtime,
/// in bevaluate()), or those that contain free variables (because
/// equality is not knowable, until those variables are grounded.)
///
/// This explicitly prevents the insertion of
///    (Equals (Number 2) (Number 3))
/// into the AtomSpace, but still allows
///    (Equals (Number 2) (Plus (Number 2) (Number 5)))
/// Now, we could check this at insertion time, because the Plus
/// has no variables, and is thus immediately evaluatable. But this
/// example is too simple: there's too many ways to voilate assorted
/// assumptions about what may or may not be executable at the time
/// that we go to insert. Thus, allow potential nonsense to be inserted.
/// I don't see a better way, just right now.
void EqualLink::setAtomSpace(AtomSpace* as)
{
	Handle id;
	for (const Handle& h: _outgoing)
	{
		if (not is_closed(h)) continue;
		if (h->is_executable()) continue;
		if (nullptr == id)
		{
			id = h;
			continue;
		}

		if (*h != *id)
			throw SyntaxException(TRACE_INFO,
				"Cannot place EqualLink with non-equal elements in the AtomSpace!  Got %s",
				to_string().c_str());
	}

	Link::setAtomSpace(as);
}

/* ================================================================= */

/// Check for semantic equality. -- Are things equal, after execution?
/// The "equal things" might be Values, as this is commonly used on
/// arithmetic expressions.
bool EqualLink::bevaluate(AtomSpace* as, bool silent)
{
	size_t nelts = _outgoing.size();
	if (2 > nelts) return true;

	ValuePtr id;
	for (const Handle& h: _outgoing)
	{
		if (h->is_type(VARIABLE_NODE)) continue;

		if (not h->is_executable())
		{
			if (nullptr == id)
			{
				id = h;
				continue;
			}

			if (*(ValueCast(h)) != *id)
				return false;

			continue;
		}

		ValuePtr vp(h->execute(as, silent));

		// If the return value is a ContainerValue, we assume that this
		// is the result of executing a MeetLink or QueryLink.
		// In this case, unwrap it, to get the "actual value".
		// This feels slightly hacky, but will do for just right now.
		if (vp->is_type(CONTAINER_VALUE))
		{
			HandleSeq hs(LinkValueCast(vp)->to_handle_seq());
			if (1 == hs.size())
				vp = hs[0];
			else
				throw RuntimeException(TRACE_INFO,
					"Expecting only one result: got %s",
					vp->to_string().c_str());
		}

		if (vp->is_atom())
			vp = as->add_atom(HandleCast(vp));

		if (nullptr == id)
		{
			id = vp;
			continue;
		}

		if (*vp != *id)
			return false;
	}
	return true;
}

DEFINE_LINK_FACTORY(EqualLink, EQUAL_LINK);

/* ===================== END OF FILE ===================== */
