/*
 * opencog/atoms/rule/VardeclOfLink.h
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

#ifndef _OPENCOG_VARDECL_OF_LINK_H
#define _OPENCOG_VARDECL_OF_LINK_H

#include <opencog/atoms/base/Link.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The VardeclOfLink returns the Variable declarations on the
/// indicated atom.
class VardeclOfLink : public Link
{
private:
	void init(void);

protected:
	Handle _vardecl;

public:
	VardeclOfLink(const HandleSeq&&, Type=VARDECL_OF_LINK);

	VardeclOfLink(const VardeclOfLink&) = delete;
	VardeclOfLink& operator=(const VardeclOfLink&) = delete;

	// Return the variable decls.
	virtual ValuePtr execute(AtomSpace*, bool);

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(VardeclOfLink)
#define createVardeclOfLink CREATE_DECL(VardeclOfLink)

/** @}*/
}

#endif // _OPENCOG_VARDECL_OF_LINK_H
