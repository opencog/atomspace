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
/**
 * The FunctionLink provides an important method: execute().
 * The execute() method performs delta-reduction on this atom, and
 * returns a Handle resulting from the execution.
 *
 * Here, "delta-reduction" is the name for the idea that certain Links
 * can be replaced by other Atoms that are operationally equivalent
 * but simpler.  For example, the delta-reduction of (Plus 2 2) is 4.
 * Delta-reduction can be understood in several ways. From the viewpoint
 * of term-rewriting, or inference or theorem-proving, a delta-reduction
 * is a (usually infinite) set of inference rules for reducing the
 * initial expression to the final expression. From the viewpoint of
 * proceedural computation, it just means "perform this computation
 * on this input", viz. "execute this function".
 *
 * The FunctionLink is meant to be a base class for any link type
 * that behaves like a function; i.e. can be executed.  Observe that
 * it derives from the FreeLink, and NOT the LambdaLink.  This may seem
 * counter-intuitive, and deserves an explanation, so here it is:
 * All link types inheriting from this class will always, by definition,
 * have their outgoing set be the arguments to that function. Think of
 * PlusLink, for example.  We don't want to insert a lambda with
 * variable declarations in there; that would just be weird and create
 * confusion.  If the arguments to PlusLink happen to include a
 * variable, that variable is necessarily free; thus, this class
 * derives from FreeLink.
 *
 * Note that this class must NOT be used for user-defined functions;
 * users should use the LambdaLink for that.
 */
class FunctionLink : public FreeLink
{
protected:
	static void check_type(Type t);
	void init(void);

public:
	// Sadly, need to make this public, else the factory code fails.
	FunctionLink(const HandleSeq&&, Type = FUNCTION_LINK);

	FunctionLink(const FunctionLink&) = delete;
	FunctionLink& operator=(const FunctionLink&) = delete;
	virtual ~FunctionLink() {}

	virtual bool is_executable(void) const { return true; }
	static Handle factory(const Handle&);
};

typedef std::shared_ptr<FunctionLink> FunctionLinkPtr;
static inline FunctionLinkPtr FunctionLinkCast(const Handle& h)
   { return std::dynamic_pointer_cast<FunctionLink>(h); }
static inline FunctionLinkPtr FunctionLinkCast(const AtomPtr& a)
   { return std::dynamic_pointer_cast<FunctionLink>(a); }
static inline FunctionLinkPtr FunctionLinkCast(const ValuePtr& a)
   { return std::dynamic_pointer_cast<FunctionLink>(a); }

#define createFunctionLink std::make_shared<FunctionLink>

/** @}*/
}

#endif // _OPENCOG_FUNCTION_LINK_H
