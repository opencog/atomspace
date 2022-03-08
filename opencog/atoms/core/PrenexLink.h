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

/// The PrenexLink extends the RewriteLink class in such a way that
/// the only allowed rewrites result in a link in that is in prenex
/// form, that is, with all variable declarations out in front,
/// and never within the body.
///
/// This is used primarily to ensure that PatternLinks remain in
/// prenex form, even when being rewritten. PatternLinks must have
/// all available declarations out in front, in order to work.
///
class PrenexLink : public RewriteLink
{
protected:
	void init(void);
	Handle reassemble(Type, const HandleMap&, const Variables&) const;

public:
	PrenexLink(const HandleSeq&&, Type=PRENEX_LINK);
	PrenexLink(const Handle& varcdecls, const Handle& body);
	PrenexLink(const PrenexLink &) = delete;
	PrenexLink& operator=(const PrenexLink &) = delete;

	virtual Handle beta_reduce(const HandleSeq& seq) const;
	virtual Handle beta_reduce(const HandleMap& vm) const;

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(PrenexLink)
#define createPrenexLink std::make_shared<PrenexLink>

/** @}*/
}

#endif // _OPENCOG_PRENEX_LINK_H
