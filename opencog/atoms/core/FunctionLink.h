/*
 * opencog/atoms/core/FunctionLink.h
 *
 * Copyright (C) 2015 Linas Vepstas
 * All Rights Reserved
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Function Software Foundation and including the exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program; if not, write to:
 * Function Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#ifndef _OPENCOG_FUNCTION_LINK_H
#define _OPENCOG_FUNCTION_LINK_H

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/core/FreeLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * The FunctionLink provides a base class with an important method:
 * execute().
 *
 * The execute() method executes the given expression, and returns
 * a Handle resulting from the execution.
 *
 * The difference between execution and reduction is this: an expression
 * that contains free variables will contain the same free variables (or
 * a subset of them) after reduction.  By contrast, it is (usually) not
 * valid to execute an expression that contains free variables; usually,
 * an error will be thrown. Thus, reduction is for open sentences,
 * execution is for closed sentences.
 *
 * Note also: EvaluationLinks can be reduced, but they can never be
 * executed (they can only be evaluated).
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
   { AtomPtr a(h); return std::dynamic_pointer_cast<FunctionLink>(a); }
static inline FunctionLinkPtr FunctionLinkCast(AtomPtr a)
   { return std::dynamic_pointer_cast<FunctionLink>(a); }

// XXX temporary hack ...
#define createFunctionLink std::make_shared<FunctionLink>

/** @}*/
}

#endif // _OPENCOG_FUNCTION_LINK_H
