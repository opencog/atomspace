/*
 * GrantLink.cc
 *
 * Copyright (C) 2015,2023 Linas Vepstas
 *
 * Author: Linas Vepstas <linasvepstas@gmail.com>  May 2015
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

#include <opencog/util/exceptions.h>
#include <opencog/util/oc_assert.h>
#include <opencog/atoms/base/ClassServer.h>

#include "GrantLink.h"

using namespace opencog;

void GrantLink::init(void)
{
	if (not nameserver().isA(get_type(), GRANT_LINK))
		throw SyntaxException(TRACE_INFO,
			"Expecting a GrantLink, got %s",
				nameserver().getTypeName(get_type()).c_str());

	// Perform some additional checks in the UniqueLink init method
	UniqueLink::init();
}

GrantLink::GrantLink(const HandleSeq&& oset, Type t)
	: UniqueLink(std::move(oset), t)
{
	init();
}

GrantLink::GrantLink(const Handle& name, const Handle& defn)
	: UniqueLink(HandleSeq({name, defn}), GRANT_LINK)
{
	init();
}

/// Content-based comparison. Due to the uniqueness constraint,
/// Two GrantLinks are "identical" if and only if the first outgoing
/// Atom is the same. The second one does not affect results.
/// But this is done only for closed grants (containing no variables);
/// the open ones do not have to be unique (as variales are needed for
/// query patterns &c.)
bool GrantLink::operator==(const Atom& other) const
{
	if (not is_closed())
		return Link::operator==(other);

	// If other points to this, then have equality.
	if (this == &other) return true;

	// Rule out obvious mis-matches, based on the hash.
	if (get_hash() != other.get_hash()) return false;
	if (get_type() != other.get_type()) return false;

	// Perform a content-compare on the first Atom in the outgoing set.
	const Handle& rhs = other.getOutgoingAtom(0);
	if (*(_outgoing[0]) != *(rhs))
		return false;

	return true;
}

/// Overload the UniqueLink::setAtomSpace() method, and just do the
/// ordinary thing. Uniqueness is done via hashing.
void GrantLink::setAtomSpace(AtomSpace* as)
{
	Atom::setAtomSpace(as);

	// Sanity check. This will trigger if the user does this:
	//    (Grant (Concept "A") (Concept "B"))
	//    (cog-push-atomspace)
	//    (Grant (Concept "A") (Concept "foo"))
	// and "foo" is not in the base space. The cog-push creates
	// a COW Frame, and in that Frame, the AtomSpace will try to
	// honor the COW and insert the second grant.
	//
	// Well ... is that OK? Should we allow it? My gut instinct says
	// no: This is supposed to be an atomic thread-safe relation;
	// allowing it to get hidden in COW spaces seems ... wrong.
	//
	// The solution is to throw a SilentException(), and to catch
	// it in the AtomSpace::add_atom() method, which then returns
	// the original atom.
	//
	// The semantics here is still dicey. The user could hide the
	// thing, by "deleting" it in a COW space, and then later, add
	// a grant with a different value. We don't check for that, we
	// don't prevent it. The correct semantics here is... unclear.
	//
	if (is_closed())
	{
		try {
			UniqueLink::setAtomSpace(as);
		} catch (...) {
			throw SilentException();
		}
	}
}

Handle GrantLink::get_link(const Handle& alias, const AtomSpace* as)
{
	return get_unique(alias, GRANT_LINK, false, as);
}

DEFINE_LINK_FACTORY(GrantLink, GRANT_LINK)

/* ===================== END OF FILE ===================== */
