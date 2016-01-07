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
/// the same first-atom causes teh previous StateLink to be removed!
///
/// This class is intended for holding single-valued state in a safe,
/// automated fashion. Of course, a user can also store unique state
/// simply by being careful to delete the old state after adding the
/// new state; but this can be error prone.  Thus link type provides
/// convenience and safety.
///
class StateLink : public UniqueLink
{
protected:
	void init();
public:
	StateLink(const HandleSeq&,
	           TruthValuePtr tv = TruthValue::DEFAULT_TV(),
	           AttentionValuePtr av = AttentionValue::DEFAULT_AV());

	StateLink(const Handle& alias, const Handle& body,
	           TruthValuePtr tv = TruthValue::DEFAULT_TV(),
	           AttentionValuePtr av = AttentionValue::DEFAULT_AV());

	StateLink(Link &l);
	Handle get_alias(void) const { return _outgoing[0]; }
	Handle get_state(void) const { return _outgoing[1]; }
	Handle get_other(void) const;

	/**
	 * Given a Handle pointing to <name> in
	 *
	 * StateLink
	 *    <name>
	 *    <body>
	 *
	 * return <body>
	 */
	static Handle get_link(const Handle& alias);
	static Handle get_state(const Handle& alias);
};

typedef std::shared_ptr<StateLink> StateLinkPtr;
static inline StateLinkPtr StateLinkCast(const Handle& h)
	{ AtomPtr a(h); return std::dynamic_pointer_cast<StateLink>(a); }
static inline StateLinkPtr StateLinkCast(AtomPtr a)
	{ return std::dynamic_pointer_cast<StateLink>(a); }

// XXX temporary hack ...
#define createStateLink std::make_shared<StateLink>

/** @}*/
}

#endif // _OPENCOG_STATE_LINK_H
