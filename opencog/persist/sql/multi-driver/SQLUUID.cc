/*
 * SQLUUID.cc
 * TLB & UUID management.
 *
 * Copyright (c) 2008,2009,2013,2017 Linas Vepstas <linas@linas.org>
 *
 * LICENSE:
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
#include <stdlib.h>
#include <unistd.h>

#include <opencog/atoms/base/Atom.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atomspaceutils/TLB.h>

#include "SQLAtomStorage.h"
#include "SQLResponse.h"

using namespace opencog;

/* ================================================================== */

void SQLAtomStorage::registerWith(AtomSpace* as)
{
	_tlbuf.set_resolver(&as->get_atomtable());

#ifdef NOT_NEEDED_RIGHT_NOW
	// The goal here is to avoid cluttering the TLB with lots of
	// extra junk that has been removed from the atomspace. This
	// can happen in several ways:
	//
	// 1) User code adds atoms to the atomspace, saves them to the
	//	database, then deletes them from the atomspace.  If this is
	//	done, then pointers to those atoms will continue on, here in
	//	the TLB, chewing up RAM.
	// 2) The above happens unintentionally, due to a bug in the code.
	//
	// The callback just deletes stuff not in the atomspace, as, chances
	// are, they'll never be used again.
	//
	_extract_sig = as->atomRemovedAtomSignal().connect(
		std::bind(&SQLAtomStorage::extract_callback, this,
			std::placeholders::_1));
#endif // NOT_NEEDED_RIGHT_NOW
}

void SQLAtomStorage::unregisterWith(AtomSpace* as)
{
	flushStoreQueue();
	_tlbuf.clear_resolver(&as->get_atomtable());

#ifdef NOT_NEEDED_RIGHT_NOW
	_extract_sig.disconnect();
#endif // NOT_NEEDED_RIGHT_NOW
}

void SQLAtomStorage::extract_callback(const AtomPtr& atom)
{
	_tlbuf.removeAtom(atom);
}

/* ================================================================== */
/// Return the UUID of the handle, if it is known.
/// If the handle is in the database, then the correct UUID is returned.
/// If the handle is NOT in the database, this returns the invalid UUID.
UUID SQLAtomStorage::check_uuid(const Handle& h)
{
	UUID uuid = _tlbuf.getUUID(h);
	if (TLB::INVALID_UUID != uuid) return uuid;

	// Optimize for bulk stores. That is, we know for a fact that
	// the database cannot possibly contain this atom yet, so do
	// not query for it!
	if (bulk_store) return TLB::INVALID_UUID;

	// Ooops. We need to look in the database to find out what this is.
	Handle dbh;
	if (h->is_node())
	{
		dbh = doGetNode(h->get_type(), h->get_name().c_str());
	}
	else
	{
		dbh = doGetLink(h->get_type(), h->getOutgoingSet());
	}
	// If it was found in the database, then the TLB got updated.
	if (dbh) return _tlbuf.getUUID(h);

	// If it was not found in the database, then say so.
	return TLB::INVALID_UUID;
}

/// Return the UUID of the handle, if it is known, else throw exception.
/// If the handle is in the database, then the correct UUID is returned.
/// If the handle is NOT in the database, this throws a silent exception.
UUID SQLAtomStorage::get_uuid(const Handle& h)
{
	UUID uuid = check_uuid(h);
	if (TLB::INVALID_UUID != uuid) return uuid;

	// Throw a silent exception; don't clutter log-files with this!
	throw NotFoundException(TRACE_INFO, "");

	return TLB::INVALID_UUID;
}

/* ================================================================ */

UUID SQLAtomStorage::getMaxObservedUUID(void)
{
	Response rp(conn_pool);
	rp.intval = 0;
	rp.exec("SELECT uuid FROM Atoms ORDER BY uuid DESC LIMIT 1;");
	rp.rs->foreach_row(&Response::intval_cb, &rp);
	return rp.intval;
}

SQLAtomStorage::VUID SQLAtomStorage::getMaxObservedVUID(void)
{
	Response rp(conn_pool);
	rp.intval = 0;
	rp.exec("SELECT vuid FROM Values ORDER BY vuid DESC LIMIT 1;");
	rp.rs->foreach_row(&Response::intval_cb, &rp);
	return rp.intval;
}

int SQLAtomStorage::getMaxObservedHeight(void)
{
	Response rp(conn_pool);
	rp.intval = 0;
	rp.exec("SELECT height FROM Atoms ORDER BY height DESC LIMIT 1;");
	rp.rs->foreach_row(&Response::intval_cb, &rp);
	return rp.intval;
}

UUID SQLAtomStorage::reserve(void)
{
	UUID max_observed_id = getMaxObservedUUID();
	logger().debug("SQLAtomStorage::reserve(): Max observed UUID is %lu\n",
		 max_observed_id);
	_tlbuf.reserve_upto(max_observed_id);
	return max_observed_id;
}

void SQLAtomStorage::clear_cache(void)
{
	_tlbuf.clear();
	reserve();
}

/* ============================= END OF FILE ================= */
