/*
 * opencog/atoms/core/PrenexLink.h
 *
 * Copyright (C) 2017 Linas Vepstas
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

#ifndef _OPENCOG_PRENEX_LINK_H
#define _OPENCOG_PRENEX_LINK_H

#include <opencog/atoms/core/RewriteLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The PrenexLink extends the ScopeLink class to add a large variety
/// of methods to rewrite various parts of the ScopeLink in various
/// ways.  These are used by the backward and foreward chainers to
/// edit and create PatternLinks on the fly, thus allowing different
/// kinds of queries to be generated and run as chaining proceeds.
///
class PrenexLink;
typedef std::shared_ptr<PrenexLink> PrenexLinkPtr;
class PrenexLink : public RewriteLink
{
protected:
	PrenexLink(Type, const Handle&);

protected:
	void init(void);

public:
	PrenexLink(const HandleSeq&, Type=PRENEX_LINK);
	PrenexLink(const Handle& varcdecls, const Handle& body);
	PrenexLink(const Link &l);

	static Handle factory(const Handle&);
};

static inline PrenexLinkPtr PrenexLinkCast(const Handle& h)
	{ return std::dynamic_pointer_cast<PrenexLink>(AtomCast(h)); }
static inline PrenexLinkPtr PrenexLinkCast(const AtomPtr& a)
	{ return std::dynamic_pointer_cast<PrenexLink>(a); }

#define createPrenexLink std::make_shared<PrenexLink>

/** @}*/
}

#endif // _OPENCOG_PRENEX_LINK_H
