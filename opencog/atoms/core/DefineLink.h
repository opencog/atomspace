/*
 * opencog/atoms/DefineLink.h
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

#ifndef _OPENCOG_DEFINE_LINK_H
#define _OPENCOG_DEFINE_LINK_H

#include <opencog/atoms/core/UniqueLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The DefineLink is used to give a name to a hypergraph (schema,
/// pattern, concept, predicate, etc).  The name is unique, in that,
/// any attempt to make a different definition with the same name will
/// throw an error.  Thus, only ONE DefineLink with a given name can
/// exist at a time.
///
/// This class is intended to be used for anything that needs to be
/// accessed by name: for, if there were two things with the same name,
/// it would be ambiguous as to which to access. (It would not make
/// sense to access both: would the result of access have 'and'
/// semantics? 'or' semantics ??)  Thus, names are unique.
///
/// This is useful for three different purposes. These are:
/// -- The definition of new predicates and schemas. Yes, the 
///    EquivalenceLink could be used for this; but right now, we are
///    experimenting with DefineLink.
/// -- A programmer/use convenience. Handy for tagging some atom with
///    a name, and then referring to that atom by it's name, later on.
/// -- Enabling recursion. A definition can occur within itself, and
///    can thus specify an infinitely-recursive pattern.  When evaluated
///    or executed, this infinite pattern must, of course terminate,
///    or your code will hang.  Bummer if your code hangs.
///
/// Of the three, the last is the most important, as, right now, there
/// is no other way of specifying recursive functions in the atomspace.
///
class DefineLink : public UniqueLink
{
protected:
	void init();
public:
	DefineLink(const HandleSeq&&, Type=DEFINE_LINK);

	DefineLink(const Handle& alias, const Handle& body);

	DefineLink(const DefineLink&) = delete;
	DefineLink& operator=(const DefineLink&) = delete;

	Handle get_alias(void) const { return _outgoing.at(0); }
	Handle get_definition(void) const { return _outgoing.at(1); }

	/**
	 * Given a Handle pointing to <name> in
	 *
	 * DefineLink
	 *    <name>
	 *    <body>
	 *
	 * return <body>
	 */
	static Handle get_definition(const Handle& alias, const AtomSpace*);
	static Handle get_definition(const Handle& alias)
	{ return get_definition(alias, alias->getAtomSpace()); }

	/**
	 * Given a Handle pointing to <name> in
	 *
	 * DefineLink
	 *    <name>
	 *    <body>
	 *
	 * return the DefineLink for the given AtomSpace.
	 */
	static Handle get_link(const Handle& alias, const AtomSpace*);

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(DefineLink)
#define createDefineLink CREATE_DECL(DefineLink)

/** @}*/
}

#endif // _OPENCOG_DEFINE_LINK_H
