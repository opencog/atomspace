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

Handle dflt_fetch_value(const Handle& h, const Handle& key);
Handle dflt_fetch_incoming_set(const Handle& h);
Handle dflt_fetch_incoming_by_type(const Handle& h, Type t);
Handle dflt_fetch_query2(const Handle& query, const Handle& key);
Handle dflt_fetch_query4(const Handle& query, const Handle& key,
                                Handle meta, bool fresh);
void dflt_store_value(const Handle& h, const Handle& key);
void dflt_update_value(const Handle& h, const Handle& key, ValuePtr delta);
void dflt_load_type(Type t);
void dflt_load_atomspace(const Handle& space);
void dflt_store_atomspace(const Handle& space);
HandleSeq dflt_load_frames(void);
void dflt_store_frames(const Handle& has);
void dflt_delete_frame(const Handle& has);
bool dflt_delete(const Handle& h);
bool dflt_delete_recursive(const Handle& h);
void dflt_barrier(void);
void dflt_erase(void);
void dflt_proxy_open(void);
void dflt_proxy_close(void);
void dflt_set_proxy(const Handle& h);
std::string dflt_monitor(void);
Handle current_storage(void);
};

#endif // _OPENCOG_PERSIST_CYTHON_H
