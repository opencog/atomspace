/*
 * opencog/persist/api/PersistSCM.cc
 *
 * Copyright (c) 2008 by OpenCog Foundation
 * Copyright (c) 2008, 2009, 2013, 2015, 2022 Linas Vepstas <linasvepstas@gmail.com>
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
#include <opencog/guile/SchemeSmob.h>
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
	define_scheme_primitive("cog-connected?",
	             &PersistSCM::connected, this, "persist");
	define_scheme_primitive("cog-storage-node",
	             &PersistSCM::current_storage, this, "persist");

	// define_scheme_primitive(..., false); means that these functions
	// will NOT be `define-public` and just plain `define`. Thus,
	// accessible within the module, but not outside of it.
	define_scheme_primitive("sn-fetch-atom",
	             &PersistSCM::sn_fetch_atom, "persist", false);
	define_scheme_primitive("sn-fetch-value",
	             &PersistSCM::sn_fetch_value, "persist", false);
	define_scheme_primitive("sn-fetch-incoming-set",
	             &PersistSCM::sn_fetch_incoming_set, "persist", false);
	define_scheme_primitive("sn-fetch-incoming-by-type",
	             &PersistSCM::sn_fetch_incoming_by_type, "persist", false);
	define_scheme_primitive("sn-fetch-query-2args",
	             &PersistSCM::sn_fetch_query2, "persist", false);
	define_scheme_primitive("sn-fetch-query-4args",
	             &PersistSCM::sn_fetch_query4, "persist", false);
	define_scheme_primitive("sn-store-atom",
	             &PersistSCM::sn_store_atom, "persist", false);
	define_scheme_primitive("sn-store-value",
	             &PersistSCM::sn_store_value, "persist", false);
	define_scheme_primitive("sn-update-value",
	             &PersistSCM::sn_update_value, "persist", false);
	define_scheme_primitive("sn-load-atoms-of-type",
	             &PersistSCM::sn_load_type, "persist", false);
	define_scheme_primitive("sn-load-atomspace",
	             &PersistSCM::sn_load_atomspace, "persist", false);
	define_scheme_primitive("sn-store-atomspace",
	             &PersistSCM::sn_store_atomspace, "persist", false);
	define_scheme_primitive("sn-load-frames",
	             &PersistSCM::sn_load_frames, "persist", false);
	define_scheme_primitive("sn-store-frames",
	             &PersistSCM::sn_store_frames, "persist", false);
	define_scheme_primitive("sn-delete-frame",
	             &PersistSCM::sn_delete_frame, "persist", false);
	define_scheme_primitive("sn-delete",
	             &PersistSCM::sn_delete, "persist", false);
	define_scheme_primitive("sn-delete-rec",
	             &PersistSCM::sn_delete_recursive, "persist", false);
	define_scheme_primitive("sn-erase",
	             &PersistSCM::sn_erase, "persist", false);
	define_scheme_primitive("sn-barrier",
	             &PersistSCM::sn_barrier, "persist", false);
	define_scheme_primitive("sn-proxy-open",
	             &PersistSCM::sn_proxy_open, "persist", false);
	define_scheme_primitive("sn-proxy-close",
	             &PersistSCM::sn_proxy_close, "persist", false);
	define_scheme_primitive("sn-set-proxy",
	             &PersistSCM::sn_set_proxy, "persist", false);
	define_scheme_primitive("sn-monitor",
	             &PersistSCM::sn_monitor, "persist", false);

	define_scheme_primitive("dflt-fetch-atom",
	             &PersistSCM::dflt_fetch_atom, this, "persist", false);
	define_scheme_primitive("dflt-fetch-value",
	             &PersistSCM::dflt_fetch_value, this, "persist", false);
	define_scheme_primitive("dflt-fetch-incoming-set",
	             &PersistSCM::dflt_fetch_incoming_set, this, "persist", false);
	define_scheme_primitive("dflt-fetch-incoming-by-type",
	             &PersistSCM::dflt_fetch_incoming_by_type, this, "persist", false);
	define_scheme_primitive("dflt-fetch-query-2args",
	             &PersistSCM::dflt_fetch_query2, this, "persist", false);
	define_scheme_primitive("dflt-fetch-query-4args",
	             &PersistSCM::dflt_fetch_query4, this, "persist", false);
	define_scheme_primitive("dflt-store-atom",
	             &PersistSCM::dflt_store_atom, this, "persist", false);
	define_scheme_primitive("dflt-store-value",
	             &PersistSCM::dflt_store_value, this, "persist", false);
	define_scheme_primitive("dflt-update-value",
	             &PersistSCM::dflt_update_value, this, "persist", false);
	define_scheme_primitive("dflt-load-atoms-of-type",
	             &PersistSCM::dflt_load_type, this, "persist", false);
	define_scheme_primitive("dflt-load-atomspace",
	             &PersistSCM::dflt_load_atomspace, this, "persist", false);
	define_scheme_primitive("dflt-store-atomspace",
	             &PersistSCM::dflt_store_atomspace, this, "persist", false);
	define_scheme_primitive("dflt-load-frames",
	             &PersistSCM::dflt_load_frames, this, "persist", false);
	define_scheme_primitive("dflt-store-frames",
	             &PersistSCM::dflt_store_frames, this, "persist", false);
	define_scheme_primitive("dflt-delete-frame",
	             &PersistSCM::dflt_delete_frame, this, "persist", false);
	define_scheme_primitive("dflt-delete",
	             &PersistSCM::dflt_delete, this, "persist", false);
	define_scheme_primitive("dflt-delete-rec",
	             &PersistSCM::dflt_delete_recursive, this, "persist", false);
	define_scheme_primitive("dflt-erase",
	             &PersistSCM::dflt_erase, this, "persist", false);
	define_scheme_primitive("dflt-barrier",
	             &PersistSCM::dflt_barrier, this, "persist", false);
	define_scheme_primitive("dflt-proxy-open",
	             &PersistSCM::dflt_proxy_open, this, "persist", false);
	define_scheme_primitive("dflt-proxy-close",
	             &PersistSCM::dflt_proxy_close, this, "persist", false);
	define_scheme_primitive("dflt-set-proxy",
	             &PersistSCM::dflt_set_proxy, this, "persist", false);
	define_scheme_primitive("dflt-monitor",
	             &PersistSCM::dflt_monitor, this, "persist", false);
}

// =====================================================================

// South Texas Nuclear Project
#define GET_STNP \
	if (not nameserver().isA(hsn->get_type(), STORAGE_NODE)) { \
		throw RuntimeException(TRACE_INFO, \
			"Expecting StorageNode, got %s", hsn->to_short_string().c_str()); \
	} \
 \
	StorageNodePtr stnp = StorageNodeCast(hsn); \
 \
	/* The cast will fail, if the dynamic library that defines the type */ \
	/* isn't loaded. This is the user's job. They can do it by saying */ \
	/* (use-modules (opencog persist-foo)) */ \
	if (nullptr == stnp) { \
		if (hsn->get_type() == STORAGE_NODE) { \
			throw RuntimeException(TRACE_INFO, \
				"A StorageNode cannot be used directly; " \
				"only it's sub-types provide the needed implementation!"); \
		} \
		throw RuntimeException(TRACE_INFO, \
			"Not opened; please load module that defines %s\n" \
			"Like so: (use-modules (opencog persist-foo))\n" \
			"where `foo` is the module providing the node.", \
			nameserver().getTypeName(hsn->get_type()).c_str()); \
	}

StorageNodePtr PersistSCM::_sn;

void PersistSCM::open(Handle hsn)
{
	GET_STNP;
	if (stnp->connected())
		throw RuntimeException(TRACE_INFO,
			"StorageNode %s is already open!",
			hsn->to_short_string().c_str());

	// It can happen that _sn was deleted earlier. Clobber
	// the smart pointer so use count goes to zero, and the
	// StorageNode dtor runs (which then closes the connection.)
	if (_sn and nullptr == _sn->getAtomSpace()) _sn = nullptr;

	// Like above, but the same StorageNode in a different AtomSpace!?
	// if (_sn and *_sn == *stnp) _sn = nullptr;

	stnp->open();

	if (nullptr == _sn) _sn = stnp;
}

void PersistSCM::close(Handle hsn)
{
	GET_STNP;
	if (not stnp->connected())
		throw RuntimeException(TRACE_INFO,
			"StorageNode %s is not open!",
			hsn->to_short_string().c_str());

	stnp->close();

	if (stnp == _sn) _sn = nullptr;
}

bool PersistSCM::connected(Handle hsn)
{
	StorageNodePtr stnp = StorageNodeCast(hsn);
	if (nullptr == stnp) return false;
	return stnp->connected();
}

// =====================================================================

Handle PersistSCM::sn_fetch_atom(Handle h, Handle hsn)
{
	GET_STNP;
	const AtomSpacePtr& asp = SchemeSmob::ss_get_env_as("fetch-atom");
	return stnp->fetch_atom(h, asp.get());
}

Handle PersistSCM::sn_fetch_value(Handle h, Handle key, Handle hsn)
{
	GET_STNP;
	const AtomSpacePtr& asp = SchemeSmob::ss_get_env_as("fetch-value");
	return stnp->fetch_value(h, key, asp.get());
}

Handle PersistSCM::sn_fetch_incoming_set(Handle h, Handle hsn)
{
	GET_STNP;
	const AtomSpacePtr& asp = SchemeSmob::ss_get_env_as("fetch-incoming-set");
	// The "false" flag here means that the fetch is NOT recursive.
	return stnp->fetch_incoming_set(h, false, asp.get());
}

Handle PersistSCM::sn_fetch_incoming_by_type(Handle h, Type t, Handle hsn)
{
	GET_STNP;
	const AtomSpacePtr& asp = SchemeSmob::ss_get_env_as("fetch-incoming-by-type");
	return stnp->fetch_incoming_by_type(h, t, asp.get());
}

Handle PersistSCM::sn_fetch_query2(Handle query, Handle key, Handle hsn)
{
	GET_STNP;
	const AtomSpacePtr& asp = SchemeSmob::ss_get_env_as("fetch-query");
	return stnp->fetch_query(query, key, Handle::UNDEFINED, false, asp.get());
}

Handle PersistSCM::sn_fetch_query4(Handle query, Handle key,
                                Handle meta, bool fresh, Handle hsn)
{
	GET_STNP;
	const AtomSpacePtr& asp = SchemeSmob::ss_get_env_as("fetch-query");
	return stnp->fetch_query(query, key, meta, fresh, asp.get());
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

void PersistSCM::sn_update_value(Handle h, Handle key, ValuePtr delta, Handle hsn)
{
	GET_STNP;
	stnp->update_value(h, key, delta);
}

void PersistSCM::sn_load_type(Type t, Handle hsn)
{
	GET_STNP;
	const AtomSpacePtr& asp = SchemeSmob::ss_get_env_as("load-atoms-of-type");
	stnp->fetch_all_atoms_of_type(t, asp.get());
}

void PersistSCM::sn_load_atomspace(Handle hsn)
{
	GET_STNP;
	const AtomSpacePtr& asp = SchemeSmob::ss_get_env_as("load-atomspace");
	stnp->load_atomspace(asp.get());
}

void PersistSCM::sn_store_atomspace(Handle hsn)
{
	GET_STNP;
	const AtomSpacePtr& asp = SchemeSmob::ss_get_env_as("store-atomspace");
	stnp->store_atomspace(asp.get());
}

HandleSeq PersistSCM::sn_load_frames(Handle hsn)
{
	GET_STNP;
	return stnp->load_frames();
}

void PersistSCM::sn_store_frames(Handle hsn, Handle has)
{
	GET_STNP;
	stnp->store_frames(has);
}

void PersistSCM::sn_delete_frame(Handle hsn, Handle has)
{
	GET_STNP;
	stnp->delete_frame(has);
}

bool PersistSCM::sn_delete(Handle h, Handle hsn)
{
	GET_STNP;
	const AtomSpacePtr& as = SchemeSmob::ss_get_env_as("cog-delete!");
	return stnp->remove_atom(as, h, false);
}

bool PersistSCM::sn_delete_recursive(Handle h, Handle hsn)
{
	GET_STNP;
	const AtomSpacePtr& as = SchemeSmob::ss_get_env_as("cog-delete-recursive!");
	return stnp->remove_atom(as, h, true);
}

void PersistSCM::sn_barrier(Handle hsn)
{
	GET_STNP;
	const AtomSpacePtr& asp = SchemeSmob::ss_get_env_as("barrier");
	stnp->barrier(asp.get());
}

void PersistSCM::sn_erase(Handle hsn)
{
	GET_STNP;
	stnp->erase();
}

void PersistSCM::sn_proxy_open(Handle hsn)
{
	GET_STNP;
	stnp->proxy_open();
}

void PersistSCM::sn_proxy_close(Handle hsn)
{
	GET_STNP;
	stnp->proxy_close();
}

void PersistSCM::sn_set_proxy(Handle h, Handle hsn)
{
	GET_STNP;
	stnp->set_proxy(h);
}

std::string PersistSCM::sn_monitor(Handle hsn)
{
	GET_STNP;
	return stnp->monitor();
}

// =====================================================================

#define CHECK \
	if (nullptr == _sn) \
		throw RuntimeException(TRACE_INFO, "No open connection to storage!");

Handle PersistSCM::dflt_fetch_atom(Handle h)
{
	CHECK;
	const AtomSpacePtr& asp = SchemeSmob::ss_get_env_as("fetch-atom");
	return _sn->fetch_atom(h, asp.get());
}

Handle PersistSCM::dflt_fetch_value(Handle h, Handle key)
{
	CHECK;
	const AtomSpacePtr& asp = SchemeSmob::ss_get_env_as("fetch-value");
	return _sn->fetch_value(h, key, asp.get());
}

Handle PersistSCM::dflt_fetch_incoming_set(Handle h)
{
	CHECK;
	const AtomSpacePtr& asp = SchemeSmob::ss_get_env_as("fetch-incoming-set");
	// The "false" flag here means that the fetch is NOT recursive.
	return _sn->fetch_incoming_set(h, false, asp.get());
}

Handle PersistSCM::dflt_fetch_incoming_by_type(Handle h, Type t)
{
	CHECK;
	const AtomSpacePtr& asp = SchemeSmob::ss_get_env_as("fetch-incoming-by-type");
	return _sn->fetch_incoming_by_type(h, t, asp.get());
}

Handle PersistSCM::dflt_fetch_query2(Handle query, Handle key)
{
	CHECK;
	const AtomSpacePtr& asp = SchemeSmob::ss_get_env_as("fetch-query");
	return _sn->fetch_query(query, key, Handle::UNDEFINED, false, asp.get());
}

Handle PersistSCM::dflt_fetch_query4(Handle query, Handle key,
                                Handle meta, bool fresh)
{
	CHECK;
	const AtomSpacePtr& asp = SchemeSmob::ss_get_env_as("fetch-query");
	return _sn->fetch_query(query, key, meta, fresh, asp.get());
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

void PersistSCM::dflt_update_value(Handle h, Handle key, ValuePtr delta)
{
	CHECK;
	_sn->update_value(h, key, delta);
}

void PersistSCM::dflt_load_type(Type t)
{
	CHECK;
	const AtomSpacePtr& asp = SchemeSmob::ss_get_env_as("load-atoms-of-type");
	_sn->fetch_all_atoms_of_type(t, asp.get());
}

void PersistSCM::dflt_load_atomspace(void)
{
	CHECK;
	const AtomSpacePtr& asp = SchemeSmob::ss_get_env_as("load-atomspace");
	_sn->load_atomspace(asp.get());
}

void PersistSCM::dflt_store_atomspace(void)
{
	CHECK;
	const AtomSpacePtr& asp = SchemeSmob::ss_get_env_as("store-atomspace");
	_sn->store_atomspace(asp.get());
}

HandleSeq PersistSCM::dflt_load_frames(void)
{
	CHECK;
	return _sn->load_frames();
}

void PersistSCM::dflt_store_frames(Handle has)
{
	CHECK;
	_sn->store_frames(has);
}

void PersistSCM::dflt_delete_frame(Handle has)
{
	CHECK;
	_sn->delete_frame(has);
}

bool PersistSCM::dflt_delete(Handle h)
{
	CHECK;
	const AtomSpacePtr& as = SchemeSmob::ss_get_env_as("cog-delete!");
	return _sn->remove_atom(as, h, false);
}

bool PersistSCM::dflt_delete_recursive(Handle h)
{
	CHECK;
	const AtomSpacePtr& as = SchemeSmob::ss_get_env_as("cog-delete-recursive!");
	return _sn->remove_atom(as, h, true);
}

void PersistSCM::dflt_barrier(void)
{
	CHECK;
	const AtomSpacePtr& asp = SchemeSmob::ss_get_env_as("barrier");
	_sn->barrier(asp.get());
}

void PersistSCM::dflt_erase(void)
{
	CHECK;
	_sn->erase();
}

void PersistSCM::dflt_proxy_open(void)
{
	CHECK;
	_sn->proxy_open();
}

void PersistSCM::dflt_proxy_close(void)
{
	CHECK;
	_sn->proxy_close();
}

void PersistSCM::dflt_set_proxy(Handle h)
{
	CHECK;
	_sn->set_proxy(h);
}

std::string PersistSCM::dflt_monitor(void)
{
	if (nullptr == _sn)
		return "No open connection to storage!";
	return _sn->monitor();
}

Handle PersistSCM::current_storage(void)
{
	return Handle(_sn);
}

void opencog_persist_init(void)
{
	static PersistSCM patty;
}

// =================== END OF FILE ====================
