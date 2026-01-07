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
 */

#ifndef _OPENCOG_FUNCTION_LINK_H
#define _OPENCOG_FUNCTION_LINK_H

#include <opencog/atoms/base/Link.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */
/**
 * The FunctionLink is meant to be a base class for any link type
 * that behaves like a function; i.e. can be executed.
 *
 * This class must NOT be used for user-defined functions;
 * users should use the LambdaLink for that.
 */
class FunctionLink : public Link
{
public:
	// Sadly, need to make this public, else the factory code fails.
	FunctionLink(const HandleSeq&&, Type = FUNCTION_LINK);

	FunctionLink(const FunctionLink&) = delete;
	FunctionLink& operator=(const FunctionLink&) = delete;
	virtual ~FunctionLink() {}

	virtual bool is_executable(void) const { return true; }

	/// Generic utility -- execute the argument, and return
	/// the result of the execution.
	static inline ValuePtr get_value(AtomSpace* as, bool silent, ValuePtr vptr)
	{
		if (not vptr->is_atom())
			return vptr;

		Handle h(HandleCast(vptr));
		if (not h->is_executable()) return vptr;
		return h->execute(as, silent);
	}

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(FunctionLink)
#define createFunctionLink CREATE_DECL(FunctionLink)

/** @}*/
}

#endif // _OPENCOG_FUNCTION_LINK_H
