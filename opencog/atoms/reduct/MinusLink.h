/*
 * opencog/atoms/reduct/MinusLink.h
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

#ifndef _OPENCOG_MINUS_LINK_H
#define _OPENCOG_MINUS_LINK_H

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/reduct/PlusLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * The MinusLink implements the mathematical operation of "minus"
 */
class MinusLink : public PlusLink
{
protected:
	void init(void);
	MinusLink(Type, const HandleSeq& oset,
	         TruthValuePtr tv = TruthValue::DEFAULT_TV(),
	         AttentionValuePtr av = AttentionValue::DEFAULT_AV());

	MinusLink(Type, const Handle& a, const Handle& b,
	         TruthValuePtr tv = TruthValue::DEFAULT_TV(),
	         AttentionValuePtr av = AttentionValue::DEFAULT_AV());

public:
	MinusLink(const Handle& a, const Handle& b,
	         TruthValuePtr tv = TruthValue::DEFAULT_TV(),
	         AttentionValuePtr av = AttentionValue::DEFAULT_AV());

	MinusLink(const HandleSeq& oset,
	         TruthValuePtr tv = TruthValue::DEFAULT_TV(),
	         AttentionValuePtr av = AttentionValue::DEFAULT_AV());
	MinusLink(Link& l);
};

typedef std::shared_ptr<MinusLink> MinusLinkPtr;
static inline MinusLinkPtr MinusLinkCast(const Handle& h)
   { AtomPtr a(h); return std::dynamic_pointer_cast<MinusLink>(a); }
static inline MinusLinkPtr MinusLinkCast(AtomPtr a)
   { return std::dynamic_pointer_cast<MinusLink>(a); }

// XXX temporary hack ...
#define createMinusLink std::make_shared<MinusLink>

/** @}*/
}

#endif // _OPENCOG_MINUS_LINK_H
