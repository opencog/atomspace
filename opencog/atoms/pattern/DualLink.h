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
public:
	DualLink(const HandleSeq&&, Type=DUAL_LINK);

	DualLink(const DualLink&) = delete;
	DualLink& operator=(const DualLink&) = delete;

	virtual bool is_executable() const { return true; }
	virtual ValuePtr execute(AtomSpace*, bool silent=false);
	static Handle factory(const Handle&);
};

LINK_PTR_DECL(DualLink)
#define createDualLink std::make_shared<DualLink>

/** @}*/
}

#endif // _OPENCOG_DUAL_LINK_H
