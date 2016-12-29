/*
 * opencog/atoms/core/FreeLink.h
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

#ifndef _OPENCOG_FREE_LINK_H
#define _OPENCOG_FREE_LINK_H

#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/core/Variables.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * The FreeLink records all of the free variables that occur within
 * (underneath) it, in traversal order. Those variables are placed in
 * sequential order in _varseq. An index is placed in _index. That is,
 * given a variable, its ordinal number is placed in _index.
 */
class FreeLink;
typedef std::shared_ptr<FreeLink> FreeLinkPtr;
class FreeLink : public Link
{
protected:
	FreeVariables _vars;

	void init(void);

public:
	FreeLink(Type, const HandleSeq& oset,
	         TruthValuePtr tv = TruthValue::DEFAULT_TV());

protected:
	// XXX Need to make this public, so that the factory can call it!
	FreeLink(Type, const Handle& a,
	         TruthValuePtr tv = TruthValue::DEFAULT_TV());

	FreeLink(Type, const Handle& a, const Handle& b,
	         TruthValuePtr tv = TruthValue::DEFAULT_TV());
public:
	FreeLink(const HandleSeq& oset,
	         TruthValuePtr tv = TruthValue::DEFAULT_TV());
	FreeLink(const Handle& a,
	         TruthValuePtr tv = TruthValue::DEFAULT_TV());

	FreeLink(Link& l);
	virtual ~FreeLink() {}

	const FreeVariables& get_vars() const
	{ return  _vars; }

	static FreeLinkPtr factory(const Handle&);
	static FreeLinkPtr factory(Type, const HandleSeq&);
};

static inline FreeLinkPtr FreeLinkCast(const Handle& h)
   { return std::dynamic_pointer_cast<FreeLink>(AtomCast(h)); }
static inline FreeLinkPtr FreeLinkCast(const AtomPtr& a)
   { return std::dynamic_pointer_cast<FreeLink>(a); }

// XXX temporary hack ...
#define createFreeLink std::make_shared<FreeLink>

/** @}*/
}

#endif // _OPENCOG_FREE_LINK_H
