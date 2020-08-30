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
///
/// FYI, this can also throw, because doGetLink() can throw, if one of
/// the Atoms in the Link is missing. So, really, this and get_uuid()
/// should be merged back into one. XXX FIXME.
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

void SQLAtomStorage::clear_cache(void)
{
	_tlbuf.clear();
}

/* ================================================================ */
/* Pool management */

/// Reset the uuid_pool sequence, if and only if we are the only
/// ones who are connected.  Otherwise, no reset can take place,
/// because we are not the only user, and we must respect the currently
/// issued UUID bundle.
void SQLAtomStorage::UUID_manager::reset_uuid_pool(UUID maxuuid)
{
	// Create the missing sequences, if they do not exist. This is
	// for backwards compatibility with older databases that do not
	// have these sequences in them.
	std::string create =
		"CREATE SEQUENCE IF NOT EXISTS " + poolname + " START WITH "
		+ std::to_string(maxuuid+1) +
		" INCREMENT BY 400;";

	Response rp(that->conn_pool);
	rp.exec(create.c_str());

	// Deal with sequence incompatibility between version 9 and 10 ...
	std::string select_increment;
	if (that->_server_version < 100000)
	{
		// Postgres version 9.5 and later
		select_increment = "SELECT increment_by FROM " + poolname;
	}
	else
	{
		// Postgres version 10
		select_increment =
		    "SELECT increment_by FROM pg_sequences WHERE sequencename = '"
		    + poolname + "'";
	}

	std::string reset =
		"DO $$"
		"DECLARE "
		"   under BIGINT;"
		"BEGIN"
		"   IF (SELECT count(*) FROM pg_stat_activity WHERE"
		"           datname=(SELECT current_database())) = "
		+ std::to_string(that->_initial_conn_pool_size) +
		" THEN"
		"      under := " + std::to_string(maxuuid + 1) +
		"            - (" + select_increment + ");"
		"      IF (1 < under) THEN "
		"         RAISE NOTICE 'Set " + poolname + " sequence to %', under;"
		"         PERFORM (SELECT setval('" + poolname + "', under));"
		"      END IF;"
		"   END IF;"
		"END $$;";

	rp.exec(reset.c_str());

	rp.intval = 0;
	rp.exec(select_increment + ";");
	rp.rs->foreach_row(&Response::intval_cb, &rp);
	_uuid_pool_increment = rp.intval;

	// Prepare to issue UUID's
	_uuid_pool_top = maxuuid;
	_next_unused_uuid = _uuid_pool_top + 1;
}

/// Issue a previously unallocated UUID.
UUID SQLAtomStorage::UUID_manager::get_uuid(void)
{
	UUID uuid = _next_unused_uuid.fetch_add(1);
	while (_uuid_pool_top < uuid)
	{
		refill_uuid_pool();
		uuid = _next_unused_uuid.fetch_add(1);
	}
	return uuid;
}

/// Obtain a block of unused UUID's from the SQL Sequence
void SQLAtomStorage::UUID_manager::refill_uuid_pool(void)
{
	Response rp(that->conn_pool);
	rp.intval = 0;
	rp.exec("SELECT nextval('" + poolname + "');");
	rp.rs->foreach_row(&Response::intval_cb, &rp);
	// top is the last one that can be issued without requiring a refill.
	_next_unused_uuid = rp.intval;
	_uuid_pool_top = rp.intval + _uuid_pool_increment - 1;
}

/* ============================= END OF FILE ================= */
