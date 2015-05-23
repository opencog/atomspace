/*
 * opencog/atoms/reduct/PutLink.h
 *
 * Copyright (C) 2015 Linas Vepstas
 * All Rights Reserved
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Put Software Foundation and including the exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program; if not, write to:
 * Put Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#ifndef _OPENCOG_PUT_LINK_H
#define _OPENCOG_PUT_LINK_H

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/reduct/FreeLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * The PutLink implements a beta redex; it is similar to an imperative
 * version of MemberLink, with argumets reversed. 
 *
 * A beta redex is a concatentation or composition of two expressions,
 * one with N free variables in it, and a list of N values for those
 * variables.  The natural form for a beta redex is in the unreduced
 * form: the values are not (yet) substituted for the variables; they
 * are simply sitting there, ready and waiting for that reduction to
 * happen.
 *
 * Thus, the PutLink makes use of the FreeLink to identify the free
 * variables.  It also overloads it's reduce() method, which implements
 * the actual beta reduction.  The reduce() method only performs the
 * substitution; it does not attempt to execute or evaluate the
 * resulting expression.
 */
class PutLink : public FreeLink
{
protected:
	void init(void);
	Handle substitute_nocheck(const Handle&, const HandleSeq&) const;
	Handle do_reduce(void) const;

	PutLink(Type, const HandleSeq& oset,
	         TruthValuePtr tv = TruthValue::NULL_TV(),
	         AttentionValuePtr av = AttentionValue::DEFAULT_AV());

	PutLink(Type, const Handle& a,
	         TruthValuePtr tv = TruthValue::NULL_TV(),
	         AttentionValuePtr av = AttentionValue::DEFAULT_AV());

	PutLink(Type, const Handle& a, const Handle& b,
	         TruthValuePtr tv = TruthValue::NULL_TV(),
	         AttentionValuePtr av = AttentionValue::DEFAULT_AV());
public:
	PutLink(const HandleSeq& oset,
	         TruthValuePtr tv = TruthValue::NULL_TV(),
	         AttentionValuePtr av = AttentionValue::DEFAULT_AV());
	PutLink(const Handle& a,
	         TruthValuePtr tv = TruthValue::NULL_TV(),
	         AttentionValuePtr av = AttentionValue::DEFAULT_AV());

	PutLink(Link& l);
	virtual ~PutLink() {}

	virtual Handle reduce(void);
};

typedef std::shared_ptr<PutLink> PutLinkPtr;
static inline PutLinkPtr PutLinkCast(const Handle& h)
   { AtomPtr a(h); return std::dynamic_pointer_cast<PutLink>(a); }
static inline PutLinkPtr PutLinkCast(AtomPtr a)
   { return std::dynamic_pointer_cast<PutLink>(a); }

// XXX temporary hack ...
#define createPutLink std::make_shared<PutLink>

/** @}*/
}

#endif // _OPENCOG_PUT_LINK_H
