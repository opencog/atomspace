/*
 * opencog/atoms/ForAllLink.h
 *
 * Copyright (C) 2015 OpenCog Foundation
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

#ifndef _OPENCOG_FORALL_LINK_H
#define _OPENCOG_FORALL_LINK_H

#include <opencog/atomspace/AtomSpace.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 *
 * This is somewhat experimental, I need to have the ForAllLink
 * inherits from LambdaLink so I can use the substitution methods for
 * the universal instantiation.
 */

class ForAllLink : public LambdaLink
{
protected:
	ForAllLink(Type, const Handle&,
	           TruthValuePtr tv = TruthValue::DEFAULT_TV(),
	           AttentionValuePtr av = AttentionValue::DEFAULT_AV());

	ForAllLink(Type, const HandleSeq&,
	           TruthValuePtr tv = TruthValue::DEFAULT_TV(),
	           AttentionValuePtr av = AttentionValue::DEFAULT_AV());

public:
	ForAllLink(const HandleSeq&,
	           TruthValuePtr tv = TruthValue::DEFAULT_TV(),
	           AttentionValuePtr av = AttentionValue::DEFAULT_AV());

	ForAllLink(const Handle& varcdecls, const Handle& body,
	           TruthValuePtr tv = TruthValue::DEFAULT_TV(),
	           AttentionValuePtr av = AttentionValue::DEFAULT_AV());

	ForAllLink(Link &l);
};

typedef std::shared_ptr<ForAllLink> ForAllLinkPtr;
static inline ForAllLinkPtr ForAllLinkCast(const Handle& h)
	{ AtomPtr a(h); return std::dynamic_pointer_cast<ForAllLink>(a); }
static inline ForAllLinkPtr ForAllLinkCast(AtomPtr a)
	{ return std::dynamic_pointer_cast<ForAllLink>(a); }

// XXX temporary hack ...
#define createForAllLink std::make_shared<ForAllLink>

/** @}*/
}

#endif // _OPENCOG_NUMBER_NODE_H
