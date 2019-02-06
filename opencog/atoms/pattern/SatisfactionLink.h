/*
 * opencog/atoms/pattern/SatisfactionLink.h
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
#ifndef _OPENCOG_SATISFACTION_LINK_H
#define _OPENCOG_SATISFACTION_LINK_H

#include <opencog/atoms/pattern/PatternLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */
class SatisfactionLink : public PatternLink
{
protected:
	void init(void);
public:
	SatisfactionLink(const HandleSeq&, Type=SATISFACTION_LINK);
	SatisfactionLink(const Link &l);

	virtual TruthValuePtr evaluate(AtomSpace*, bool);
	static Handle factory(const Handle&);
};

typedef std::shared_ptr<SatisfactionLink> SatisfactionLinkPtr;
static inline SatisfactionLinkPtr SatisfactionLinkCast(const Handle& h)
	{ AtomPtr a(h); return std::dynamic_pointer_cast<SatisfactionLink>(a); }
static inline SatisfactionLinkPtr SatisfactionLinkCast(AtomPtr a)
	{ return std::dynamic_pointer_cast<SatisfactionLink>(a); }

#define createSatisfactionLink std::make_shared<SatisfactionLink>

/** @}*/
}

#endif // _OPENCOG_SATISFACTION_LINK_H
