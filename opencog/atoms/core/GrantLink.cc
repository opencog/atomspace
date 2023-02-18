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

	// Must have name and body
	if (2 != _outgoing.size())
		throw SyntaxException(TRACE_INFO,
			"Expecting name and definition, got size %d", _outgoing.size());

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
	if (*((AtomPtr)_outgoing[0]) != *((AtomPtr)rhs))
		return false;

	return true;
}

/// We hash only the first Atom in the OutgoingSet; it is the one
/// that determines uniqueness. The second Atom does not matter.
/// But this is done only for closed Atoms; there is no uniqueness
/// constraint on open grants (containing variables). This allows
/// GrantLinks to appear in query terms.
ContentHash GrantLink::compute_hash() const
{
	if (not is_closed())
		return Link::compute_hash();

   // The nameserver().getTypeHash() returns hash of the type name
	// string, and is thus independent of all other type declarations.
	// 1<<44 - 377 is prime
	ContentHash hsh = ((1ULL<<44) - 377) * nameserver().getTypeHash(get_type());

	const Handle& h(_outgoing[0]);

	hsh += (hsh <<5) ^ (353 * h->get_hash()); // recursive!

	// Bit-mixing copied from murmur64. Yes, this is needed.
	hsh ^= hsh >> 33;
	hsh *= 0xff51afd7ed558ccdL;
	hsh ^= hsh >> 33;
	hsh *= 0xc4ceb9fe1a85ec53L;
	hsh ^= hsh >> 33;

	// Links will always have the MSB set.
	ContentHash mask = ((ContentHash) 1ULL) << (8*sizeof(ContentHash) - 1);
	hsh |= mask;

	if (Handle::INVALID_HASH == hsh) hsh -= 1;
	return hsh;
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
	// Well ... is that OK, and should we allow it? Or should it be
	// prevented? I don't know. An argument could be made either way.
	// Currently, there are no users who need it this or that way.
	//
	// To allow the hiding of grants, just allow the COW, delete the
	// code below, and do nothing. To disable grant hiding, let the
	// throw go through (better yet, convert it to a SilentException
	// or similar), and catch it in the AtomSpace::add_atom() method,
	// and just ignore the failed add, return the original. Not hard
	// to do.
	//
	// Either solution works. Disallowing COW hiding seems better.
	//
	if (is_closed())
	{
		try {
			UniqueLink::setAtomSpace(as);
		} catch (...) {
			OC_ASSERT(false, "Internal Error: Not allowed right now!")
		}
	}
}

/**
 * Get the definition associated with the alias.
 * This will be the second atom of some GrantLink, where
 * `alias` is the first.
 */
Handle GrantLink::get_definition(const Handle& alias, const AtomSpace* as)
{
	Handle uniq(get_unique(alias, GRANT_LINK, false, as));
	return uniq->getOutgoingAtom(1);
}

Handle GrantLink::get_link(const Handle& alias, const AtomSpace* as)
{
	return get_unique(alias, GRANT_LINK, false, as);
}

DEFINE_LINK_FACTORY(GrantLink, GRANT_LINK)

/* ===================== END OF FILE ===================== */
