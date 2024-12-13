/*
 * opencog/persist/api/cython/PersistCython.cc
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

#include <opencog/atomspace/AtomSpace.h>
#include "PersistCython.h"
#include "opencog/cython/opencog/Utilities.h"

using namespace opencog;

namespace opencog {

// XXX FIXME: except for the error messages, most of this code is
// mostly a cut-n-pate of what's in PersistSCM.cc

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
	/* from opencog.storage-foo import * */ \
	if (nullptr == stnp) { \
		if (hsn->get_type() == STORAGE_NODE) { \
			throw RuntimeException(TRACE_INFO, \
				"A StorageNode cannot be used directly; " \
				"only it's sub-types provide the needed implementation!"); \
		} \
		throw RuntimeException(TRACE_INFO, \
			"Not opened; please load module that defines %s\n" \
			"Like so: from opencog.storage-foo import *\n" \
			"where `foo` is the module providing the node.", \
			nameserver().getTypeName(hsn->get_type()).c_str()); \
	}

// =====================================================================

// Single global default storage node. This is used whenever the
// API does not provide an explicit storage node to use.
StorageNodePtr _sn;

void storage_open(const Handle& hsn)
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

void storage_close(const Handle& hsn)
{
	GET_STNP;
	if (not stnp->connected())
		throw RuntimeException(TRACE_INFO,
			"StorageNode %s is not open!",
			hsn->to_short_string().c_str());

	stnp->close();

	if (stnp == _sn) _sn = nullptr;
}

bool storage_connected(const Handle& hsn)
{
	StorageNodePtr stnp = StorageNodeCast(hsn);
	if (nullptr == stnp) return false;
	return stnp->connected();
}

// =====================================================================

#define CHECK \
	if (nullptr == _sn) \
		throw RuntimeException(TRACE_INFO, "No open connection to storage!");


/**
 * Store the single atom to the backing store hanging off the
 * atom-space.
 */
Handle dflt_store_atom(const Handle& h)
{
	CHECK;
	_sn->store_atom(h);
	return h;
}

Handle dflt_fetch_atom(const Handle& h)
{
	CHECK;
	AtomSpace* as = get_context_atomspace();
	return _sn->fetch_atom(h, as);
}

Handle dflt_fetch_value(const Handle& h, const Handle& key)
{
	CHECK;
	AtomSpace* as = get_context_atomspace();
	return _sn->fetch_value(h, key, as);
}

Handle dflt_fetch_incoming_set(const Handle& h)
{
	CHECK;
	AtomSpace* as = get_context_atomspace();
	// The "false" flag here means that the fetch is NOT recursive.
	return _sn->fetch_incoming_set(h, false, as);
}

Handle dflt_fetch_incoming_by_type(const Handle& h, Type t)
{
	CHECK;
	AtomSpace* as = get_context_atomspace();
	return _sn->fetch_incoming_by_type(h, t, as);
}

Handle dflt_fetch_query2(const Handle& query, const Handle& key)
{
	CHECK;
	AtomSpace* as = get_context_atomspace();
	return _sn->fetch_query(query, key, Handle::UNDEFINED, false, as);
}

Handle dflt_fetch_query4(const Handle& query, const Handle& key,
                                Handle meta, bool fresh)
{
	CHECK;
	AtomSpace* as = get_context_atomspace();
	return _sn->fetch_query(query, key, meta, fresh, as);
}

void dflt_store_value(const Handle& h, const Handle& key)
{
	CHECK;
	_sn->store_value(h, key);
}

void dflt_update_value(const Handle& h, const Handle& key, ValuePtr delta)
{
	CHECK;
	_sn->update_value(h, key, delta);
}

void dflt_load_type(Type t)
{
	CHECK;
	AtomSpace* as = get_context_atomspace();
	_sn->fetch_all_atoms_of_type(t, as);
}

void dflt_load_atomspace(const Handle& space)
{
	CHECK;
	if (space and space->get_type() == ATOM_SPACE)
		_sn->load_atomspace(AtomSpaceCast(space).get());
	else
	{
		AtomSpace* as = get_context_atomspace();
		_sn->load_atomspace(as);
	}
}

void dflt_store_atomspace(const Handle& space)
{
	CHECK;
	if (space and space->get_type() == ATOM_SPACE)
		_sn->store_atomspace(AtomSpaceCast(space).get());
	else
	{
		AtomSpace* as = get_context_atomspace();
		_sn->store_atomspace(as);
	}
}

HandleSeq dflt_load_frames(void)
{
	CHECK;
	return _sn->load_frames();
}

void dflt_store_frames(const Handle& has)
{
	CHECK;
	_sn->store_frames(has);
}

void dflt_delete_frame(const Handle& has)
{
	CHECK;
	_sn->delete_frame(has);
}

bool dflt_delete(const Handle& h)
{
	CHECK;
	AtomSpace* as = get_context_atomspace();
	return _sn->remove_atom(as, h, false);
}

bool dflt_delete_recursive(const Handle& h)
{
	CHECK;
	AtomSpace* as = get_context_atomspace();
	return _sn->remove_atom(as, h, true);
}

void dflt_barrier(void)
{
	CHECK;
	AtomSpace* as = get_context_atomspace();
	_sn->barrier(as);
}

void dflt_erase(void)
{
	CHECK;
	_sn->erase();
}

void dflt_proxy_open(void)
{
	CHECK;
	_sn->proxy_open();
}

void dflt_proxy_close(void)
{
	CHECK;
	_sn->proxy_close();
}

void dflt_set_proxy(const Handle& h)
{
	CHECK;
	_sn->set_proxy(h);
}

std::string dflt_monitor(void)
{
	if (nullptr == _sn)
		return "No open connection to storage!";
	return _sn->monitor();
}

Handle current_storage(void)
{
	return Handle(_sn);
}
};

// =================== END OF FILE ====================
