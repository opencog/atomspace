/*
 * opencog/atoms/GrantLink.h
 *
 * Copyright (C) 2015, 2023 Linas Vepstas
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

#ifndef _OPENCOG_GRANT_LINK_H
#define _OPENCOG_GRANT_LINK_H

#include <opencog/atoms/core/UniqueLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The GrantLink is used to create unique, thread-safe, mutually-
/// exclusive, atomic, first-come-first-serve relationships between
/// pairs of Atoms. The first Atom in a GrantLink can be thought of
/// as a name; the rest as a definition. Once a name has been "granted",
/// it cannot be re-used again. Attempts to create a second GrantLink
/// (of the same name) will simply return the first.  No errors are
/// thrown.
///
/// The intended use of the GrantLink is to provide for thread-safe
/// unique relationships between pairs of Atoms, without risk of thread
/// races creating two relationships for the same thing.
///
class GrantLink : public UniqueLink
{
protected:
	void init(void);
	virtual ContentHash compute_hash() const;
	virtual void setAtomSpace(AtomSpace*);

public:
	GrantLink(const HandleSeq&&, Type=GRANT_LINK);

	GrantLink(const Handle& alias, const Handle& body);

	GrantLink(const GrantLink&) = delete;
	GrantLink& operator=(const GrantLink&) = delete;

	/** Content-based compare. */
	virtual bool operator==(const Atom&) const;

	Handle get_alias(void) const { return _outgoing.at(0); }
	Handle get_definition(void) const { return _outgoing.at(1); }

	/**
	 * Given a Handle pointing to <name> in
	 *
	 * GrantLink
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
	 * GrantLink
	 *    <name>
	 *    <body>
	 *
	 * return the GrantLink for the given AtomSpace.
	 */
	static Handle get_link(const Handle& alias, const AtomSpace*);

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(GrantLink)
#define createGrantLink CREATE_DECL(GrantLink)

/** @}*/
}

#endif // _OPENCOG_GRANT_LINK_H
