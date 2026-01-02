/*
 * opencog/atoms/grant/PipeLink.h
 *
 * Copyright (C) 2015 Linas Vepstas
 * Copyright (C) 2026 BrainyBlaze Dynamics, LLC
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
 */

#ifndef _OPENCOG_PIPE_LINK_H
#define _OPENCOG_PIPE_LINK_H

#include <opencog/atoms/grant/DefineLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The PipeLink is used to assign names to data flows. It is a
/// convenience, used for wiring up data flow networks.
///
/// Only one name per data flow is allowed; any attempt to attach the
/// same name to another data stream will throw an exception.  Only
/// one PipeLink with a given name can exist at a time; to change the
/// assoociation, the original PipeLink must be deleted first.
///
class PipeLink : public UniqueLink
{
protected:
	void init(void);
public:
	PipeLink(const HandleSeq&&, Type=PIPE_LINK);

	PipeLink(const PipeLink&) = delete;
	PipeLink& operator=(const PipeLink&) = delete;

	Handle get_alias(void) const { return _outgoing.at(0); }
	Handle get_stream(void) const { return _outgoing.at(1); }

	/**
	 * Given a Handle pointing to <name> in
	 *
	 * PipeLink
	 *    <name>
	 *    <stream>
	 *
	 * return <stream>
	 */
	static Handle get_stream(const Handle& alias, const AtomSpace*);
	static Handle get_stream(const Handle& alias)
	{ return get_stream(alias, alias->getAtomSpace()); }

	/**
	 * Given a Handle pointing to <name> in
	 *
	 * PipeLink
	 *    <name>
	 *    <stream>
	 *
	 * return the PipeLink for the given AtomSpace.
	 */
	static Handle get_link(const Handle& alias, const AtomSpace*);

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(PipeLink)
#define createPipeLink CREATE_DECL(PipeLink)

/** @}*/
}

#endif // _OPENCOG_PIPE_LINK_H
