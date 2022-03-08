/*
 * opencog/atoms/parallel/ExecuteThreadedLink.h
 *
 * Copyright (C) 2020 Linas Vepstas
 * SPDX-License-Identifier: AGPL-3.0-or-later
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

#ifndef _OPENCOG_EXECUTE_THREADED_LINK_H
#define _OPENCOG_EXECUTE_THREADED_LINK_H

#include <opencog/atoms/base/Link.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

class AtomSpace;

class ExecuteThreadedLink : public Link
{
protected:
	size_t _nthreads;
	size_t _setoff;

public:
	ExecuteThreadedLink(const HandleSeq&&, Type=EXECUTE_THREADED_LINK);
	ExecuteThreadedLink(const ExecuteThreadedLink&) = delete;
	ExecuteThreadedLink& operator=(const ExecuteThreadedLink&) = delete;

	virtual bool is_executable() const { return true; }
	virtual ValuePtr execute(AtomSpace*, bool);

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(ExecuteThreadedLink)
#define createExecuteThreadedLink CREATE_DECL(ExecuteThreadedLink)

/** @}*/
}

#endif // _OPENCOG_EXECUTE_THREADED_LINK_H
