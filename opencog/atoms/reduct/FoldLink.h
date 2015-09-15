/*
 * opencog/atoms/reduct/FoldLink.h
 *
 * Copyright (C) 2015 Linas Vepstas
 * All Rights Reserved
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Fold Software Foundation and including the exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program; if not, write to:
 * Fold Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#ifndef _OPENCOG_FOLD_LINK_H
#define _OPENCOG_FOLD_LINK_H

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/core/FunctionLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * The FoldLink implements the generic reduction of lists, by
 * cons'ing together elements of the same type. See
 * http://en.wikipedia.org/wiki/Fold_(higher-order_function)
 * for a general discussion.
 */
class FoldLink : public FunctionLink
{
protected:
	Handle knil;
	virtual Handle kons(const Handle&, const Handle&) = 0;
	Type distributive_type = NOTYPE;

	void init(void);
	FoldLink(Type, const HandleSeq& oset,
	         TruthValuePtr tv = TruthValue::NULL_TV(),
	         AttentionValuePtr av = AttentionValue::DEFAULT_AV());

	FoldLink(Type, const Handle& a, const Handle& b,
	         TruthValuePtr tv = TruthValue::NULL_TV(),
	         AttentionValuePtr av = AttentionValue::DEFAULT_AV());
public:
	FoldLink(const HandleSeq& oset,
	         TruthValuePtr tv = TruthValue::NULL_TV(),
	         AttentionValuePtr av = AttentionValue::DEFAULT_AV());
	FoldLink(Link& l);

   virtual Handle reduce(void);

	static LinkPtr factory(LinkPtr);
	static Handle factory(Type, const HandleSeq&);
};

typedef std::shared_ptr<FoldLink> FoldLinkPtr;
static inline FoldLinkPtr FoldLinkCast(const Handle& h)
   { AtomPtr a(h); return std::dynamic_pointer_cast<FoldLink>(a); }
static inline FoldLinkPtr FoldLinkCast(AtomPtr a)
   { return std::dynamic_pointer_cast<FoldLink>(a); }

// XXX temporary hack ...
#define createFoldLink std::make_shared<FoldLink>

/** @}*/
}

#endif // _OPENCOG_FOLD_LINK_H
