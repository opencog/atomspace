/*
 * opencog/persist/api/PersistSCM.cc
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

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/guile/SchemePrimitive.h>
#include "PersistSCM.h"

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
	define_scheme_primitive("cog-open",
	             &PersistSCM::open, this, "persist");
	define_scheme_primitive("cog-close",
	             &PersistSCM::close, this, "persist");

	define_scheme_primitive("sn-fetch-atom",
	             &PersistSCM::sn_fetch_atom, "persist");
	define_scheme_primitive("sn-fetch-value",
	             &PersistSCM::sn_fetch_value, "persist");
	define_scheme_primitive("sn-fetch-incoming-set",
	             &PersistSCM::sn_fetch_incoming_set, "persist");
	define_scheme_primitive("sn-fetch-incoming-by-type",
	             &PersistSCM::sn_fetch_incoming_by_type, "persist");
	define_scheme_primitive("sn-fetch-query-2args",
	             &PersistSCM::sn_fetch_query2, "persist");
	define_scheme_primitive("sn-fetch-query-4args",
	             &PersistSCM::sn_fetch_query4, "persist");
	define_scheme_primitive("sn-store-atom",
	             &PersistSCM::sn_store_atom, "persist");
	define_scheme_primitive("sn-store-value",
	             &PersistSCM::sn_store_value, "persist");
	define_scheme_primitive("sn-load-atoms-of-type",
	             &PersistSCM::sn_load_type, "persist");
	define_scheme_primitive("sn-load-atomspace",
	             &PersistSCM::sn_load_atomspace, "persist");
	define_scheme_primitive("sn-store-atomspace",
	             &PersistSCM::sn_store_atomspace, "persist");
	define_scheme_primitive("sn-barrier",
	             &PersistSCM::sn_barrier, "persist");

	define_scheme_primitive("dflt-fetch-atom",
	             &PersistSCM::dflt_fetch_atom, this, "persist");
	define_scheme_primitive("dflt-fetch-value",
	             &PersistSCM::dflt_fetch_value, this, "persist");
	define_scheme_primitive("dflt-fetch-incoming-set",
	             &PersistSCM::dflt_fetch_incoming_set, this, "persist");
	define_scheme_primitive("dflt-fetch-incoming-by-type",
	             &PersistSCM::dflt_fetch_incoming_by_type, this, "persist");
	define_scheme_primitive("dflt-fetch-query-2args",
	             &PersistSCM::dflt_fetch_query2, this, "persist");
	define_scheme_primitive("dflt-fetch-query-4args",
	             &PersistSCM::dflt_fetch_query4, this, "persist");
	define_scheme_primitive("dflt-store-atom",
	             &PersistSCM::dflt_store_atom, this, "persist");
	define_scheme_primitive("dflt-store-value",
	             &PersistSCM::dflt_store_value, this, "persist");
	define_scheme_primitive("dflt-load-atoms-of-type",
	             &PersistSCM::dflt_load_type, this, "persist");
	define_scheme_primitive("dflt-load-atomspace",
	             &PersistSCM::dflt_load_atomspace, this, "persist");
	define_scheme_primitive("dflt-store-atomspace",
	             &PersistSCM::dflt_store_atomspace, this, "persist");
	define_scheme_primitive("dflt-barrier",
	             &PersistSCM::dflt_barrier, this, "persist");
}

// =====================================================================

// South Texas Nuclear Project
#define GET_STNP \
	if (not nameserver().isA(hsn->get_type(), STORAGE_NODE)) \
		throw RuntimeException(TRACE_INFO, \
			"Expecting StorageNode, got %s", hsn->to_short_string().c_str()); \
 \
	StorageNodePtr stnp = StorageNodeCast(hsn); \
 \
	/* The cast will fail, if the dynamic library that defines the type */ \
	/* isn't loaded. This is the user's job. They can do it by saying */ \
	/* (use-modules (opencog persist-foo) */ \
	if (nullptr == stnp) \
		throw RuntimeException(TRACE_INFO, \
			"Not opened; please load module that defines %s\n" \
			"Like so: (use-modules (persist-foo))", \
			nameserver().getTypeName(hsn->get_type()).c_str());

StorageNodePtr PersistSCM::_sn;

void PersistSCM::open(Handle hsn)
{
	GET_STNP;
	stnp->open();

	if (nullptr == _sn) _sn = stnp;
}

void PersistSCM::close(Handle hsn)
{
	GET_STNP;
	stnp->close();

	if (stnp == _sn) _sn = nullptr;
}

// =====================================================================

Handle PersistSCM::sn_fetch_atom(Handle h, Handle hsn)
{
	GET_STNP;
	return stnp->fetch_atom(h);
}

Handle PersistSCM::sn_fetch_value(Handle h, Handle key, Handle hsn)
{
	GET_STNP;
	return stnp->fetch_value(h, key);
}

Handle PersistSCM::sn_fetch_incoming_set(Handle h, Handle hsn)
{
	GET_STNP;
	// The "false" flag here means that the fetch is NOT recursive.
	return stnp->fetch_incoming_set(h, false);
}

Handle PersistSCM::sn_fetch_incoming_by_type(Handle h, Type t, Handle hsn)
{
	GET_STNP;
	return stnp->fetch_incoming_by_type(h, t);
}

Handle PersistSCM::sn_fetch_query2(Handle query, Handle key, Handle hsn)
{
	GET_STNP;
	return stnp->fetch_query(query, key, Handle::UNDEFINED, false);
}

Handle PersistSCM::sn_fetch_query4(Handle query, Handle key,
                                Handle meta, bool fresh, Handle hsn)
{
	GET_STNP;
	return stnp->fetch_query(query, key, meta, fresh);
}

/**
 * Store the single atom to the backing store hanging off the
 * atom-space.
 */
Handle PersistSCM::sn_store_atom(Handle h, Handle hsn)
{
	GET_STNP;
	stnp->store_atom(h);
	return h;
}

void PersistSCM::sn_store_value(Handle h, Handle key, Handle hsn)
{
	GET_STNP;
	stnp->store_value(h, key);
}

void PersistSCM::sn_load_type(Type t, Handle hsn)
{
	GET_STNP;
	stnp->fetch_all_atoms_of_type(t);
}

void PersistSCM::sn_load_atomspace(Handle hsn)
{
	GET_STNP;
	stnp->load_atomspace();
}

void PersistSCM::sn_store_atomspace(Handle hsn)
{
	GET_STNP;
	stnp->store_atomspace();
}

void PersistSCM::sn_barrier(Handle hsn)
{
	GET_STNP;
	stnp->barrier();
}

// =====================================================================

#define CHECK \
	if (nullptr == _sn) \
		throw RuntimeException(TRACE_INFO, "No open connection to storage!");

Handle PersistSCM::dflt_fetch_atom(Handle h)
{
	CHECK;
	return _sn->fetch_atom(h);
}

Handle PersistSCM::dflt_fetch_value(Handle h, Handle key)
{
	CHECK;
	return _sn->fetch_value(h, key);
}

Handle PersistSCM::dflt_fetch_incoming_set(Handle h)
{
	CHECK;
	// The "false" flag here means that the fetch is NOT recursive.
	return _sn->fetch_incoming_set(h, false);
}

Handle PersistSCM::dflt_fetch_incoming_by_type(Handle h, Type t)
{
	CHECK;
	return _sn->fetch_incoming_by_type(h, t);
}

Handle PersistSCM::dflt_fetch_query2(Handle query, Handle key)
{
	CHECK;
	return _sn->fetch_query(query, key, Handle::UNDEFINED, false);
}

Handle PersistSCM::dflt_fetch_query4(Handle query, Handle key,
                                Handle meta, bool fresh)
{
	CHECK;
	return _sn->fetch_query(query, key, meta, fresh);
}

/**
 * Store the single atom to the backing store hanging off the
 * atom-space.
 */
Handle PersistSCM::dflt_store_atom(Handle h)
{
	CHECK;
	_sn->store_atom(h);
	return h;
}

void PersistSCM::dflt_store_value(Handle h, Handle key)
{
	CHECK;
	_sn->store_value(h, key);
}

void PersistSCM::dflt_load_type(Type t)
{
	CHECK;
	_sn->fetch_all_atoms_of_type(t);
}

void PersistSCM::dflt_load_atomspace(void)
{
	CHECK;
	_sn->load_atomspace();
}

void PersistSCM::dflt_store_atomspace(void)
{
	CHECK;
	_sn->store_atomspace();
}

void PersistSCM::dflt_barrier(void)
{
	CHECK;
	_sn->barrier();
}

void opencog_persist_init(void)
{
	static PersistSCM patty;
}

// =================== END OF FILE ====================
