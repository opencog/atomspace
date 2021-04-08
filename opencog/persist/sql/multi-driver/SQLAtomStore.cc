/*
 * SQLAtomStore.cc
 * Save of individual atoms.
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

/* ================================================================ */

#define BUFSZ 250
#define STMT(colname,val) { \
	if (notfirst) { cols += ", "; vals += ", "; } else notfirst = true; \
	cols += colname; \
	vals += val; \
}

#define STMTI(colname,ival) { \
	char buff[BUFSZ]; \
	snprintf(buff, BUFSZ, "%d", ival); \
	STMT(colname, buff); \
}

#define STMTF(colname,fval) { \
	char buff[BUFSZ]; \
	snprintf(buff, BUFSZ, "%22.16g", fval); \
	STMT(colname, buff); \
}

/* ================================================================ */
/**
 * Recursively store the indicated atom and all of the values attached
 * to it.  Also store it's outgoing set, and all of the values attached
 * to those atoms.  The recursive store is unconditional.
 *
 * By default, the actual store is done asynchronously (in a different
 * thread); this routine merely queues up the atom. If the synchronous
 * flag is set, then the store is performed in this thread, and it is
 * completed (sent to the Postgres server) before this method returns.
 */
void SQLAtomStorage::storeAtom(const Handle& h, bool synchronous)
{
	rethrow();

	// If a synchronous store, avoid the queues entirely.
	if (synchronous)
	{
		if (not_yet_stored(h)) do_store_atom(h);
		store_atom_values(h);
		return;
	}
	// _write_queue.enqueue(h);
	_write_queue.insert(h);
}

/**
 * Synchronously store a single atom. That is, the actual store is done
 * in the calling thread.  All values attached to the atom are also
 * stored.
 *
 * Returns the height of the atom.
 */
int SQLAtomStorage::do_store_atom(const Handle& h)
{
	if (h->is_node())
	{
		do_store_single_atom(h, 0);
		return 0;
	}

	int lheight = 0;
	for (const Handle& ho: h->getOutgoingSet())
	{
		// Recurse.
		int heig = do_store_atom(ho);
		if (lheight < heig) lheight = heig;
	}

	// Height of this link is, by definition, one more than tallest
	// atom in outgoing set.
	lheight ++;
	do_store_single_atom(h, lheight);
	return lheight;
}

void SQLAtomStorage::vdo_store_atom(const Handle& h)
{
	try
	{
		if (not_yet_stored(h)) do_store_atom(h);
		store_atom_values(h);
	}
	catch (const NotFoundException& ex)
	{
		// No-op. This happens when atoms stores and atom deletes are
		// racing with each-other. Provoked by MultiDeleteUTest.
		// get_uuid() is throwing, because the atom has been deleted.
	}
	catch (...)
	{
		_async_write_queue_exception = std::current_exception();
	}
}

/* ================================================================ */
/**
 * Return true if we don't yet have a UUID for this atom.
 * Note that it MUST take the _store_mutex lock, as otherwise one
 * thread might be trying to store a valuation for a UUID that got
 * issued, but for which the atom itself has not yet been stored.
 * This inversion of stores *will* cause the database to throw a
 * foreign-key-constraint error when it sees the valuation without
 * the corresponding atom.
 */
bool SQLAtomStorage::not_yet_stored(const Handle& h)
{
	std::lock_guard<std::mutex> create_lock(_store_mutex);
	return TLB::INVALID_UUID == _tlbuf.getUUID(h);
}

/**
 * Store just this one single atom.
 * Atoms in the outgoing set are NOT stored!
 * The store is performed synchronously (in the calling thread).
 */
void SQLAtomStorage::do_store_single_atom(const Handle& h, int aheight)
{
	setup_typemap();

	std::unique_lock<std::mutex> create_lock(_store_mutex);

	UUID uuid = check_uuid(h);
	if (TLB::INVALID_UUID != uuid) return;

	// If it was not found, then issue a brand-spankin new UUID.
	uuid = _tlbuf.addAtom(h, TLB::INVALID_UUID);

	std::string uuidbuff = std::to_string(uuid);

	bool notfirst = false;
	std::string cols;
	std::string vals;
	std::string coda;

	cols = "INSERT INTO Atoms (";
	vals = ") VALUES (";
	coda = ");";

	STMT("uuid", uuidbuff);

	// Keep performance stats
	if (0 == aheight) {
		_num_node_inserts++;
	} else {
		_num_link_inserts++;
	}

	// Store the atomspace UUID
	AtomSpace * at = h->getAtomSpace();
	// We allow storage of atoms that don't belong to an atomspace.
	if (at) uuidbuff = std::to_string(at->get_uuid());
	else uuidbuff = "0";

	// XXX FIXME -- right now, multiple space support is incomplete,
	// the below hacks around some testing issues.
	if (at) uuidbuff = "1";
	STMT("space", uuidbuff);

	// Store the atom UUID
	Type t = h->get_type();
	int dbtype = storing_typemap[t];
	STMTI("type", dbtype);

	// Store the node name, if its a node
	if (0 == aheight)
	{
		// Use postgres $-quoting to make unicode strings
		// easier to deal with.
		std::string qname = " $ocp$";
		qname += h->get_name();
		qname += "$ocp$ ";

		// The Atoms table has a UNIQUE constraint on the
		// node name.  If a node name is too long, a postgres
		// error is generated:
		// ERROR: index row size 4440 exceeds maximum 2712
		// for index "atoms_type_name_key"
		// There's not much that can be done about this, without
		// a redesign of the table format, in some way. Maybe
		// we could hash the long node names, store the hash,
		// and make sure that is unique.
		if (2700 < qname.size())
		{
			throw IOException(TRACE_INFO,
				"Error: do_store_single_atom: Maxiumum Node name size is 2700.\n");
		}
		STMT("name", qname);

		// Nodes have a height of zero by definition.
		STMTI("height", 0);
	}
	else
	{
		if (max_height < aheight) max_height = aheight;
		STMTI("height", aheight);

		if (h->is_link())
		{
			// The Atoms table has a UNIQUE constraint on the
			// outgoing set.  If a link is too large, a postgres
			// error is generated:
			// ERROR: index row size 4440 exceeds maximum 2712
			// for index "atoms_type_outgoing_key"
			// A simple, reasonable design is presented in the README
			// and in a github issue, that is easy to implement.
			// It basically defines a fake atom type, used for
			// continuation of the long list. If this is spotted,
			// then we know to get more atoms.
			if (330 < h->get_arity())
			{
				throw IOException(TRACE_INFO,
					"Error: do_store_single_atom: Maxiumum Link size is 330. "
					"Atom was: %s\n", h->to_string().c_str());
			}

			cols += ", outgoing";
			vals += ", ";
			vals += oset_to_string(h->getOutgoingSet());
		}
	}

	// In a multi-user scenario, it can happen that multiple users
	// attempt to INSERT the same atom at the same time. Only one
	// user will win; the losers will get an exception.  The losers
	// catch the exception, then they fetch the atom, and find out
	// what UUID the winner got, and then use that henceforth (to
	// store TV's, for example).
	std::string qry = cols + vals + coda;

	try
	{
		Response rp(conn_pool);
		rp.try_exec(qry.c_str());
	}
	catch (const SilentException& ex)
	{
		_tlbuf.removeAtom(uuid);
		create_lock.unlock();
		do_store_single_atom(h, aheight);
	}

#if 0
	bool try_again = false;
	// We may have to store the atom table UUID and try again...
	// We waste CPU cycles to store the atomtable, only if it failed.
	// XXX this is currently dead code ...
	if (try_again)
	{
		AtomSpace *at = h->getAtomSpace();
		if (at) store_atomtable_id(at->get_uuid());

		Response rp(conn_pool);
		rp.exec(qry.c_str());
	}
#endif

	_store_count ++;

	if (bulk_store and _store_count%100000 == 0)
	{
		time_t secs = time(0) - bulk_start;
		double rate = ((double) _store_count) / secs;
		unsigned long kays = ((unsigned long) _store_count) / 1000;
		printf("\tStored %luK atoms in %d seconds (%d per second)\n",
			kays, (int) secs, (int) rate);
	}
}

/* ============================= END OF FILE ================= */
