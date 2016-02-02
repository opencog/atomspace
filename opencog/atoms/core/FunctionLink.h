/*
 * opencog/atoms/core/FunctionLink.h
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

#ifndef _OPENCOG_FUNCTION_LINK_H
#define _OPENCOG_FUNCTION_LINK_H

#include <opencog/atoms/core/FreeLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */
class AtomSpace;

/**
 * The FunctionLink provides an important method: execute().
 * The execute() method executes the given expression, and returns
 * a Handle resulting from the execution.
 *
 * The FunctionLink is meant to be a base class for any link type
 * that behaves like a function; i.e. can be executed.  Observe that
 * it derives from the FreeLink, and NOT the LambdaLink.  This may seem
 * counter-intuitive, and deserves an explanation, so here it is:
 * All link types inheriting from this class will always, by definition,
 * have thier outgoing set be the arguments to that function. Think of
 * PlusLink, for example.  Having a lambda with variable declarations
 * in there would just be weird and create confusion.  If the arguments
 * to PlusLink happen to include a variable, that variable is necessarily
 * free; thus, this class dervies from FreeLink.
 *
 * Note that this class must NOT be used for user-defined functions;
 * users should use the LambdaLink for that.
 */
class FunctionLink : public FreeLink
{
protected:
	void init(void);
	FunctionLink(Type, const Handle& a,
	             TruthValuePtr tv = TruthValue::DEFAULT_TV(),
	             AttentionValuePtr av = AttentionValue::DEFAULT_AV());

	FunctionLink(Type, const Handle& a, const Handle& b,
	             TruthValuePtr tv = TruthValue::DEFAULT_TV(),
	             AttentionValuePtr av = AttentionValue::DEFAULT_AV());

public:
	FunctionLink(Type, const HandleSeq& oset,
	             TruthValuePtr tv = TruthValue::DEFAULT_TV(),
	             AttentionValuePtr av = AttentionValue::DEFAULT_AV());

	FunctionLink(Link& l);
	virtual ~FunctionLink() {}

	virtual Handle execute(AtomSpace* = NULL) const;
	static Handle do_execute(AtomSpace*, const Handle&);

	static LinkPtr factory(LinkPtr);
	static Handle factory(Type, const HandleSeq&);
};

typedef std::shared_ptr<FunctionLink> FunctionLinkPtr;
static inline FunctionLinkPtr FunctionLinkCast(const Handle& h)
   { return std::dynamic_pointer_cast<FunctionLink>(AtomCast(h)); }
static inline FunctionLinkPtr FunctionLinkCast(const AtomPtr& a)
   { return std::dynamic_pointer_cast<FunctionLink>(a); }

// XXX temporary hack ...
#define createFunctionLink std::make_shared<FunctionLink>

/** @}*/
}

#endif // _OPENCOG_FUNCTION_LINK_H
