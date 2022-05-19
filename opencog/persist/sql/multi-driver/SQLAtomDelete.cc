/*
 * SQLAtomDelete.cc
 * Deletion of individual atoms.
 *
 * Copyright (c) 2017 Linas Vepstas <linas@linas.org>
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
#include <opencog/atoms/atom_types/NameServer.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/persist/tlb/TLB.h>

#include "SQLAtomStorage.h"
#include "SQLResponse.h"

using namespace opencog;

#define BUFSZ 120

/* ================================================================ */

void SQLAtomStorage::deleteSingleAtom(Response& rp, UUID uuid)
{
	char buff[BUFSZ];

	// The Atom being deleted might be a foreign key in the valuations
	// table. So clobber all of those...
	// XXX We have not index on this, so it will be slow.
	snprintf(buff, BUFSZ,
		"DELETE FROM Valuations WHERE key = %lu;", uuid);
	rp.exec(buff);

#if UGLY_AND_UNDESIRED
	// This is commented out because I'm not sure of what to do.
	// This is a complex problem. We have two choices: walk the
	// values table, (this is a recursive walk), find all atoms
	// in it, and clobber them. Alternately, we can do nothing,
	// here, just leave the Atoms, where they will sit, forever.
	// Handle the failed restore elsewhere.

	// Argh. It might be a value or a valuation.
	snprintf(buff, BUFSZ,
		"DELETE FROM Valuations WHERE linkvalue = \'{%lu}\';", uuid);
	rp.exec(buff);

	// Yikes! This is wrong, because it might be recursively embedded
	// deep down in some structure! This will cause a crash! The user
	// will be unhappy! FIXME! (OK, its been like five years and no
	// one has ever done this before, and its late at night...)
	snprintf(buff, BUFSZ,
		"DELETE FROM Values WHERE linkvalue = \'{%lu}\';", uuid);
	rp.exec(buff);
#endif

	// The uuid is the PRIMARY KEY so below should be fast...
	snprintf(buff, BUFSZ,
		"DELETE FROM Atoms WHERE uuid = %lu;", uuid);

	rp.exec(buff);

	_num_atom_deletes++;
}

/// Remove an atom, and all of it's associated values from the database.
/// If the atom has a non-empty incoming set, then it is NOT removed
/// unless the recursive flag is set. If the recursive flag is set, then
/// the atom, and everything in its incoming set is removed.
///
/// TODO The deletion is performed synchronously, in single-threaded
/// fashion.  It might make sense to create a queue for this, so that
/// if could run in parallel (in the unusual case that someone has
/// millions of atoms to delete and is impateint about it...)
void SQLAtomStorage::removeAtom(AtomSpace* frm, const Handle& h, bool recursive)
{
	// Synchronize. The atom that we are deleting might be sitting
	// in the store queue.
	flushStoreQueue();
	Response rp(conn_pool);

	// Its possible that we were asked to delete an atom that we
	// don't know about. If so, there is nothing to do. Please note
	// that check_uuid does look in the database to see if the atom
	// is there, so if it's returning an invalid uuid, then it is
	// not in the database.
	UUID uuid = check_uuid(h);
	if (TLB::INVALID_UUID == uuid) return;

	// Use a transaction so that the update looks atomic to other users.
	// This has no effect on performance, from what I can tell.
	rp.exec("BEGIN;");
	removeAtom(rp, uuid, recursive);
	rp.exec("COMMIT;");
	_num_atom_removes++;
}

/// Delete ALL of the values associated with an atom.
void SQLAtomStorage::deleteAllValuations(Response& rp, UUID uuid)
{
	// There is an index on just the atom, so the below should be fast.
	char buff[BUFSZ];
	snprintf(buff, BUFSZ,
		"SELECT key FROM Valuations WHERE atom = %lu;", uuid);

	std::vector<UUID> uset;
	rp.uvec = &uset;
	rp.exec(buff);

	rp.rs->foreach_row(&Response::get_uuid_cb, &rp);

	for (UUID kuid : uset)
		deleteValuation(rp, kuid, uuid);
}

void SQLAtomStorage::removeAtom(Response& rp, UUID uuid, bool recursive)
{
	// Verify the status of the incoming set.
	// This uses the GIN index and so it should be fast.
	// CREATE INDEX incoming_idx on Atoms USING GIN(outgoing);
	char buff[BUFSZ];
	if (recursive)
	{
		snprintf(buff, BUFSZ,
			"SELECT uuid FROM Atoms WHERE outgoing @> ARRAY[CAST(%lu AS BIGINT)];",
			uuid);
	}
	else
	{
		// For non-recursive removes, we just want to check if there
		// any atoms at all, in the incoming set. So just check for
		// anything greater than zero. This check is much much faster
		// than getting all of them (above).
		snprintf(buff, BUFSZ,
			"SELECT uuid FROM Atoms WHERE outgoing @> ARRAY[CAST(%lu AS BIGINT)] LIMIT 1;",
			uuid);
	}

	std::vector<UUID> uset;
	rp.uvec = &uset;
	rp.exec(buff);
	rp.rs->foreach_row(&Response::get_uuid_cb, &rp);

	// Knock out the incoming set, first.
	if (recursive)
	{
		for (UUID iud : uset)
			removeAtom(rp, iud, recursive);
	}
	else if (0 < uset.size())
	{
		// Non-recursive deletes with non-empty incoming sets
		// are no-ops.
		return;
	}

	// Next, knock out the values.
	deleteAllValuations(rp, uuid);

	// Now, remove the atom itself.
	deleteSingleAtom(rp, uuid);

	// Finally, remove from the TLB.
	_tlbuf.removeAtom(uuid);
}

/* ============================= END OF FILE ================= */
