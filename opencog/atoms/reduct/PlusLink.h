/*
 * opencog/atoms/reduct/PlusLink.h
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

#ifndef _OPENCOG_PLUS_LINK_H
#define _OPENCOG_PLUS_LINK_H

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/reduct/ArithmeticLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * The PlusLink implements the mathematical operation of "plus"
 */
class PlusLink : public ArithmeticLink
{
protected:
	virtual double konsd(double, double) const;
	virtual Handle kons(const Handle&, const Handle&);

	void init(void);
	PlusLink(Type, const Handle& a, const Handle& b);

public:
	PlusLink(const Handle& a, const Handle& b);
	PlusLink(const HandleSeq&, Type=PLUS_LINK);
	PlusLink(const Link&);

	static Handle factory(const Handle&);
};

typedef std::shared_ptr<PlusLink> PlusLinkPtr;
static inline PlusLinkPtr PlusLinkCast(const Handle& h)
   { AtomPtr a(h); return std::dynamic_pointer_cast<PlusLink>(a); }
static inline PlusLinkPtr PlusLinkCast(AtomPtr a)
   { return std::dynamic_pointer_cast<PlusLink>(a); }

// XXX temporary hack ...
#define createPlusLink std::make_shared<PlusLink>

/** @}*/
}

#endif // _OPENCOG_PLUS_LINK_H
