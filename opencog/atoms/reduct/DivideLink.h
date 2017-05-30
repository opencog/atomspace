/*
 * opencog/atoms/reduct/DivideLink.h
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

#ifndef _OPENCOG_DIVIDE_LINK_H
#define _OPENCOG_DIVIDE_LINK_H

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/reduct/TimesLink.h>
#include <opencog/atoms/NumberNode.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * The DivideLink implements the mathematical operation of "divide".
 */
class DivideLink : public TimesLink
{
protected:
	void init(void);
	DivideLink(Type, const Handle& a, const Handle& b);

	virtual Handle do_execute(AtomSpace*, const HandleSeq&) const;
public:
	DivideLink(const Handle& a, const Handle& b);
	DivideLink(const HandleSeq& oset, Type=DIVIDE_LINK);
	DivideLink(const Link& l);

	static Handle factory(const Handle&);
};

typedef std::shared_ptr<DivideLink> DivideLinkPtr;
static inline DivideLinkPtr DivideLinkCast(const Handle& h)
   { AtomPtr a(h); return std::dynamic_pointer_cast<DivideLink>(a); }
static inline DivideLinkPtr DivideLinkCast(AtomPtr a)
   { return std::dynamic_pointer_cast<DivideLink>(a); }

// XXX temporary hack ...
#define createDivideLink std::make_shared<DivideLink>

/** @}*/
}

#endif // _OPENCOG_DIVIDE_LINK_H
