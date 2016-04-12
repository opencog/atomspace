/*
 * opencog/atoms/pattern/DualLink.h
 *
 * Copyright (C) 2015, 2016 Linas Vepstas
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
#ifndef _OPENCOG_DUAL_LINK_H
#define _OPENCOG_DUAL_LINK_H

#include <opencog/atoms/pattern/PatternLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */
class DualLink : public PatternLink
{
protected:
	void init(void);

	DualLink(Type, const HandleSeq&,
	         TruthValuePtr tv = TruthValue::DEFAULT_TV(),
	         AttentionValuePtr av = AttentionValue::DEFAULT_AV());

public:
	DualLink(const HandleSeq&,
	         TruthValuePtr tv = TruthValue::DEFAULT_TV(),
	         AttentionValuePtr av = AttentionValue::DEFAULT_AV());

	DualLink(Link &l);
};

typedef std::shared_ptr<DualLink> DualLinkPtr;
static inline DualLinkPtr DualLinkCast(const Handle& h)
	{ AtomPtr a(h); return std::dynamic_pointer_cast<DualLink>(a); }
static inline DualLinkPtr DualLinkCast(AtomPtr a)
	{ return std::dynamic_pointer_cast<DualLink>(a); }

// XXX temporary hack ...
#define createDualLink std::make_shared<DualLink>

/** @}*/
}

#endif // _OPENCOG_DUAL_LINK_H
