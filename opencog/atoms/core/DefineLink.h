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

/// The DefineLink is used to define procedures, predicates and schemas.
/// It is intended to allow procedures, predicates and schemas to be
/// invoked by name, instead of anonymously.
///
/// Only one definition is allowed; any attempt to create a second
/// conflicting definition (of the same name) will throw an error.
/// Only one DefineLink with a given name can exist at a time; to
/// change a definition, the original DefineLink must be deleted first.
///
/// DefineLinks enable the construction of recursive procedures, schemas
/// and predicates. A defined procedure can refer to itself by name.
///
/// The GrantLink is similar, except that it allows any pair of atoms
/// to be associated, with the first acting as a name. GrantLinks will
/// not throw an error if an attempt is made to redefine them; instead,
/// the original defintion will be returned. Thus, GrantLinks are used
/// to create race-free first-come-first-served exclusive (mutex)
/// relationships.
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
