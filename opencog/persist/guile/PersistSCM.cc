/*
 * opencog/persist/guile/PersistSCM.cc
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

namespace opencog
{
/** \addtogroup grp_persist
 *  @{
 */

class PersistSCM : public ModuleWrap
{
private:
	void init(void);

	Handle fetch_atom(Handle);
	ValuePtr fetch_value(Handle, Handle);
	Handle fetch_incoming_set(Handle);
	Handle fetch_incoming_by_type(Handle, Type);
	ValuePtr fetch_query(Handle, Handle, Handle, bool);
	Handle store_atom(Handle);
	void store_value(Handle, Handle);
	void load_type(Type);
	void load_atomspace(void);
	void store_atomspace(void);
	void barrier(void);

public:
	PersistSCM(void);
}; // class

/** @}*/
}  // namespace

extern "C" {
void opencog_persist_init(void);
};

#endif // _OPENCOG_PERSIST_SCM_H

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/guile/SchemePrimitive.h>

using namespace opencog;

PersistSCM::PersistSCM(void)
	: ModuleWrap("opencog persist")
{
	static bool is_init = false;
	if (is_init) return;
	is_init = true;
	module_init();
}

void PersistSCM::init(void)
{
	define_scheme_primitive("fetch-atom",
	             &PersistSCM::fetch_atom, this, "persist");
	define_scheme_primitive("fetch-value",
	             &PersistSCM::fetch_value, this, "persist");
	define_scheme_primitive("fetch-incoming-set",
	             &PersistSCM::fetch_incoming_set, this, "persist");
	define_scheme_primitive("fetch-incoming-by-type",
	             &PersistSCM::fetch_incoming_by_type, this, "persist");
	define_scheme_primitive("fetch-query-internal",
	             &PersistSCM::fetch_query, this, "persist");
	define_scheme_primitive("store-atom",
	             &PersistSCM::store_atom, this, "persist");
	define_scheme_primitive("store-value",
	             &PersistSCM::store_value, this, "persist");
	define_scheme_primitive("load-atoms-of-type",
	             &PersistSCM::load_type, this, "persist");
	define_scheme_primitive("load-atomspace",
	             &PersistSCM::load_atomspace, this, "persist");
	define_scheme_primitive("store-atomspace",
	             &PersistSCM::store_atomspace, this, "persist");
	define_scheme_primitive("barrier",
	             &PersistSCM::barrier, this, "persist");
}

// =====================================================================

Handle PersistSCM::fetch_atom(Handle h)
{
	AtomSpace *as = SchemeSmob::ss_get_env_as("fetch-atom");
	h = as->fetch_atom(h);
	return h;
}

ValuePtr PersistSCM::fetch_value(Handle h, Handle key)
{
	AtomSpace *as = SchemeSmob::ss_get_env_as("fetch-value");
	ValuePtr vp = as->fetch_value(h, key);
	return vp;
}

Handle PersistSCM::fetch_incoming_set(Handle h)
{
	// The "false" flag here means that the fetch is NOT recursive.
	AtomSpace *as = SchemeSmob::ss_get_env_as("fetch-incoming-set");
	h = as->fetch_incoming_set(h, false);
	return h;
}

Handle PersistSCM::fetch_incoming_by_type(Handle h, Type t)
{
	AtomSpace *as = SchemeSmob::ss_get_env_as("fetch-incoming-by-type");
	h = as->fetch_incoming_by_type(h, t);
	return h;
}

ValuePtr PersistSCM::fetch_query(Handle query, Handle key,
                                 Handle meta, bool fresh)
{
	AtomSpace *as = SchemeSmob::ss_get_env_as("fetch-query");
	ValuePtr vp = as->fetch_query(query, key, meta, fresh);
	return vp;
}

/**
 * Store the single atom to the backing store hanging off the
 * atom-space.
 */
Handle PersistSCM::store_atom(Handle h)
{
	AtomSpace *as = SchemeSmob::ss_get_env_as("store-atom");
	as->store_atom(h);
	return h;
}

void PersistSCM::store_value(Handle h, Handle key)
{
	AtomSpace *as = SchemeSmob::ss_get_env_as("store-value");
	as->store_value(h, key);
}

void PersistSCM::load_type(Type t)
{
	AtomSpace *as = SchemeSmob::ss_get_env_as("load-atoms-of-type");
	as->fetch_all_atoms_of_type(t);
}

void PersistSCM::load_atomspace(void)
{
	AtomSpace *as = SchemeSmob::ss_get_env_as("load-atomspace");
	as->load_atomspace();
}

void PersistSCM::store_atomspace(void)
{
	AtomSpace *as = SchemeSmob::ss_get_env_as("store-atomspace");
	as->store_atomspace();
}

void PersistSCM::barrier(void)
{
	AtomSpace *as = SchemeSmob::ss_get_env_as("barrier");
	as->barrier();
}

void opencog_persist_init(void)
{
	static PersistSCM patty;
}
