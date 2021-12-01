/*
 * opencog/atoms/StateLink.h
 *
 * Copyright (C) 2015 Linas Vepstas
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

#ifndef _OPENCOG_STATE_LINK_H
#define _OPENCOG_STATE_LINK_H

#include <opencog/atoms/core/UniqueLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The StateLink is used to maintain unique state. Given an atom,
/// the atomspace can only contain one instance of a StateLink with
/// that atom in the first position.  Adding another StateLink with
/// the same first-atom causes the previous StateLink to be removed!
///
/// This class is intended for holding single-valued state in a
/// thread-safe, fully-automated fashion. Of course, a user can also
/// store unique state simply by being careful to delete the old state
/// after adding the new state; but this would not be thread-safe.
///
/// By "thread-safe", it is meant that any other thread observing the
/// AtomSpace will only see one StateLink: either the old one, or the
/// new one; they will never see two StateLinks, and they will never
/// see zero StateLinks.
///
class StateLink : public UniqueLink
{
protected:
	void init();
	virtual void install();
public:
	StateLink(const HandleSeq&&, Type=STATE_LINK);
	StateLink(const Handle& alias, const Handle& body);

	StateLink(const StateLink&) = delete;
	StateLink& operator=(const StateLink&) = delete;
	Handle get_alias(void) const { return _outgoing.at(0); }
	Handle get_state(void) const { return _outgoing.at(1); }

	/**
	 * Return false, if the state contains a variable.
	 * The atomspace can contain multiple open StateLinks,
	 * but must never have more than one closed StateLink.
	 */
	bool is_closed(void) const { return 0 == _vars.varseq.size(); }

	/**
	 * Given a Handle pointing to <name> in
	 *
	 * StateLink
	 *    <name>
	 *    <body>
	 *
	 * return <body>. Throws exception if there is no such StateLink.
	 */
	static Handle get_state(const Handle& alias, const AtomSpace*);
	static Handle get_state(const Handle& alias)
	{ return get_state(alias, alias->getAtomSpace()); }

	/**
	 * Given a Handle pointing to <name> in
	 *
	 * StateLink
	 *    <name>
	 *    <body>
	 *
	 * return the whole StateLink. Throws exception if there is
	 * no such StateLink.
	 */
	static Handle get_link(const Handle& alias, const AtomSpace*);
	static Handle get_link(const Handle& alias)
	{ return get_link(alias, alias->getAtomSpace()); }

	/**
	 * Non-static version of the above. Uses `this->get_alias()`
	 * and then tries to find the appropriate closed link.
	 * Unlike the above, it won't throw, if not found. Instead,
	 * it will just return `this`.
	 */
	Handle get_link(const AtomSpace*);

	static Handle factory(const Handle&);
};

typedef std::shared_ptr<StateLink> StateLinkPtr;
static inline StateLinkPtr StateLinkCast(const Handle& h)
	{ return std::dynamic_pointer_cast<StateLink>(h); }
static inline StateLinkPtr StateLinkCast(const AtomPtr& a)
	{ return std::dynamic_pointer_cast<StateLink>(a); }

#define createStateLink std::make_shared<StateLink>

/** @}*/
}

#endif // _OPENCOG_STATE_LINK_H
