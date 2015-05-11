/*
 * opencog/atoms/reduct/FunctionLink.h
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
#include <opencog/atoms/reduct/FreeLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * The FunctionLink provides a base class with two important methods:
 * reduce() and execute().
 *
 * The reduce() method takes the given expression, and applies
 * term reduction rules to obtain a smaller but equivalent expression.
 * There is no guarantee that reduce is normalizing or strongly
 * normalizing, but that does seem like a desirable goal.
 *
 * The execute() method executes the given expression, and returns
 * a Handle resulting from the execution.
 *
 * The difference between execution and reduction is this: an expression
 * that contains free variables will contain the same free variables (or
 * a subset of them) after reduction.  By contrast, it is not valid to
 * execute an expression that contains free variables; an error will be
 * thrown. Thus, reduction is for open sentences, execution is for
 * closed sentences.
 *
 * If this distinction seems a bit arbitrary, well, it is.  Right now,
 * maintaining this distinction leads to a convenient implementation,
 * but that may change.
 */
class FunctionLink : public FreeLink
{
protected:
	FunctionLink(Type, const HandleSeq& oset,
	             TruthValuePtr tv = TruthValue::NULL_TV(),
	             AttentionValuePtr av = AttentionValue::DEFAULT_AV());

	FunctionLink(Type, const Handle& a, const Handle& b,
	             TruthValuePtr tv = TruthValue::NULL_TV(),
	             AttentionValuePtr av = AttentionValue::DEFAULT_AV());
public:
	FunctionLink(const HandleSeq& oset,
	             TruthValuePtr tv = TruthValue::NULL_TV(),
	             AttentionValuePtr av = AttentionValue::DEFAULT_AV());
	FunctionLink(const Handle& a,
	             TruthValuePtr tv = TruthValue::NULL_TV(),
	             AttentionValuePtr av = AttentionValue::DEFAULT_AV());

	FunctionLink(Link& l);
	virtual ~FunctionLink() {}

	virtual Handle reduce(void);
	virtual Handle execute(AtomSpace* = NULL) const;
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
