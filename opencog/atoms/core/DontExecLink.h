/*
 * opencog/atoms/core/DontExecLink.h
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

#ifndef _OPENCOG_DONT_EXEC_LINK_H
#define _OPENCOG_DONT_EXEC_LINK_H

#include <opencog/atoms/base/Link.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The DontExecLink returns whatever Atom it wraps (without executing
/// that Atom). It act as a firewall, preventing execution beyond that
/// point.
///
class DontExecLink : public Link
{
public:
	DontExecLink(const HandleSeq&&, Type=DONT_EXEC_LINK);
	DontExecLink(const DontExecLink&) = delete;
	DontExecLink& operator=(const DontExecLink&) = delete;

	virtual bool is_executable() const { return true; }
	virtual ValuePtr execute(AtomSpace*, bool) { return _outgoing[0]; }

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(DontExecLink)
#define createDontExecLink CREATE_DECL(DontExecLink)

/** @}*/
}

#endif // _OPENCOG_DONT_EXEC_LINK_H
