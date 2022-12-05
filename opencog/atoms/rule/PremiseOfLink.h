/*
 * opencog/atoms/rule/PremiseOfLink.h
 *
 * Copyright (C) 2018, 2022 Linas Vepstas
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

#ifndef _OPENCOG_PREMISE_OF_LINK_H
#define _OPENCOG_PREMISE_OF_LINK_H

#include <opencog/atoms/base/Link.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The PremiseOfLink returns the n'th premise on the given RuleLink
class PremiseOfLink : public Link
{
private:
	void init(void);

protected:
	Handle _premise;

public:
	PremiseOfLink(const HandleSeq&&, Type=PREMISE_OF_LINK);

	PremiseOfLink(const PremiseOfLink&) = delete;
	PremiseOfLink& operator=(const PremiseOfLink&) = delete;

	// Return the n'th premise of the rule.
	virtual ValuePtr execute(AtomSpace*, bool);

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(PremiseOfLink)
#define createPremiseOfLink CREATE_DECL(PremiseOfLink)

/** @}*/
}

#endif // _OPENCOG_PREMISE_OF_LINK_H
