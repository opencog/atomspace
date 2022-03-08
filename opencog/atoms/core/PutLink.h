/*
 * opencog/atoms/core/PutLink.h
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

#ifndef _OPENCOG_PUT_LINK_H
#define _OPENCOG_PUT_LINK_H

#include <opencog/atoms/core/PrenexLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * The PutLink implements a beta redex; it is similar to an imperative
 * version of MemberLink, with arguments reversed.
 *
 * A beta redex is a concatenation or composition of a combinator, and
 * a list of arguments.  Typically, the combinator will be a LambdaLink,
 * typically with N declared variables in it. To go with it, the PutLink
 * expects a list of N arguments to be plugged in for these variables.
 * Thus, for example:
 * (Put (Lambda (Variable "x") ...stuff...) (Concept "foo"))
 * when reduced, will plug "foo" into ...stuff...
 *
 * Another supported form for the combinator is any of the
 * FunctionLinks, e.g. the PlusLink, TimesLink, etc. in which case,
 * one can write (Put (Plus) (List (Number 2) (Number 2))) as the beta
 * redex that, when reduced, will return (Number 4). Fancier forms for
 * the combinator are allowed: for example,
 * (Put (Plus (Number 6) (Number 8)) (List (Number 5) (Number 9)))
 *
 * The natural form for a beta redex is in the unreduced
 * form: the arguments are not (yet) substituted for the variables; they
 * are simply sitting there, ready and waiting for that reduction to
 * happen.
 *
 * It defines a reduce() method, which implements the actual beta
 * reduction.  The reduce() method only performs the substitution; it
 * does not attempt to execute or evaluate the resulting expression.
 *
 * The implementation for PutLink is not a "pure" beta-redex, but
 * has extra stuff to make it play nice with GetLink.  That is, we'd
 * like to pipe the output of GetLink directly into PutLink as input,
 * but for one problem: GetLink returns it's stuff wrapped in SetLink's.
 * So, in order to play nice, we automatically unwrap these, beta reduce
 * them, and then wrap the answers back up with a SetLink.
 *
 * Another "enhancement" is that when there are N>1 variables, the
 * arguments must be wrapped in a ListLink, to be consistent with other
 * parts of atomese. However, for N=1, the ListLink is optional.
 */
class PutLink : public PrenexLink
{
protected:
	/// The arguments that are to be placed into the body.
	Handle _arguments;

	void init(void);
	void static_typecheck_arguments(void);

	Handle do_reduce(void) const;

public:
	PutLink(const HandleSeq&&, Type=PUT_LINK);
	PutLink(const PutLink&) = delete;
	PutLink& operator=(const PutLink&) = delete;
	virtual ~PutLink() {}

	// PutLink arguments may be the second or the third outgoing-set elt.
	Handle get_arguments() { return _arguments; }

	virtual bool is_executable() const { return true; }
	virtual ValuePtr execute(AtomSpace*, bool silent=false);

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(PutLink)
#define createPutLink std::make_shared<PutLink>

/** @}*/
}

#endif // _OPENCOG_PUT_LINK_H
