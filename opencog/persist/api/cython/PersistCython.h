/*
 * opencog/persist/api/cython/PersistCython.h
 *
 * Copyright (c) 2022,2024 Linas Vepstas <linasvepstas@gmail.com>
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

#ifndef _OPENCOG_PERSIST_CYTHON_H
#define _OPENCOG_PERSIST_CYTHON_H

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/persist/api/StorageNode.h>

using namespace opencog;

namespace opencog {
void storage_open(const Handle&);
void storage_close(const Handle&);

bool storage_connected(const Handle&);
Handle dflt_store_atom(const Handle&);
Handle dflt_fetch_atom(const Handle&);
};

#endif // _OPENCOG_PERSIST_CYTHON_H
