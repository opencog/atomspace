/*
 * opencog/persist/api/PersistSCM.h
 *
 * Copyright (c) 2008 by OpenCog Foundation
 * Copyright (c) 2008, 2009, 2013, 2015 Linas Vepstas <linasvepstas@gmail.com>
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

#ifndef _OPENCOG_PERSIST_SCM_H
#define _OPENCOG_PERSIST_SCM_H

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/guile/SchemeModule.h>
#include <opencog/persist/api/StorageNode.h>

namespace opencog
{
/** \addtogroup grp_persist
 *  @{
 */

class PersistSCM : public ModuleWrap
{
private:
	void init(void);

	// Single global default storage node,
	// which all the functions below use.
	static StorageNodePtr _sn;

	void sn_open(Handle);
	void sn_close(Handle);

	Handle sn_fetch_atom(Handle);
	Handle sn_fetch_value(Handle, Handle);
	Handle sn_fetch_incoming_set(Handle);
	Handle sn_fetch_incoming_by_type(Handle, Type);
	Handle sn_fetch_query2(Handle, Handle);
	Handle sn_fetch_query4(Handle, Handle, Handle, bool);
	Handle sn_store_atom(Handle);
	void sn_store_value(Handle, Handle);
	void sn_load_type(Type);
	void sn_load_atomspace(void);
	void sn_store_atomspace(void);
	void sn_barrier(void);

public:
	PersistSCM(void);
	static void set_connection(const StorageNodePtr& sn) { _sn = sn; }
}; // class

/** @}*/
}  // namespace

extern "C" {
void opencog_persist_init(void);
};

#endif // _OPENCOG_PERSIST_SCM_H
