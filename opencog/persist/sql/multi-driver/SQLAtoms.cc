/*
 * SQLAtoms.cc
 * Save and restore of individual atoms.
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
#include <opencog/atoms/proto/NameServer.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/base/Node.h>
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
	if (not_yet_stored(h)) do_store_atom(h);
	store_atom_values(h);
}

/* ================================================================ */

void SQLAtomStorage::deleteSingleAtom(Response& rp, UUID uuid)
{
	// The uuid is the PRIMARY KEY so below shou;ld be fast...
	char buff[BUFSZ];
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
void SQLAtomStorage::removeAtom(const Handle& h, bool recursive)
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

	std::lock_guard<std::mutex> create_lock(_store_mutex);

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

#ifdef STORAGE_DEBUG
	if (0 == aheight) {
		_num_node_inserts++;
	} else {
		_num_link_inserts++;
	}
#endif // STORAGE_DEBUG

	// Store the atomspace UUID
	AtomTable * at = getAtomTable(h);
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
			// The simplest solution that I see requires a database
			// redesign.  One could hash together the UUID's in the
			// outgoing set, and then force a unique constraint on
			// the hash.
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

	// We may have to store the atom table UUID and try again...
	// We waste CPU cycles to store the atomtable, only if it failed.
	bool try_again = false;
	std::string qry = cols + vals + coda;
	{
		Response rp(conn_pool);
		rp.exec(qry.c_str());
		if (NULL == rp.rs) try_again = true;
	}

	if (try_again)
	{
		AtomTable *at = getAtomTable(h);
		if (at) store_atomtable_id(*at);

		Response rp(conn_pool);
		rp.exec(qry.c_str());
	}

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

/* ================================================================ */
/**
 * One-size-fits-all atom fetcher.
 * Given an SQL query string, this will return a single atom.
 * It does NOT fetch values.
 */
SQLAtomStorage::PseudoPtr SQLAtomStorage::getAtom(const char * query, int height)
{
	Response rp(conn_pool);
	rp.uuid = TLB::INVALID_UUID;
	rp.exec(query);
	rp.rs->foreach_row(&Response::create_atom_cb, &rp);

	// Did we actually find anything?
	// DO NOT USE IsInvalidHandle() HERE! It won't work, duhh!
	if (rp.uuid == TLB::INVALID_UUID) return nullptr;

	rp.height = height;
	PseudoPtr atom(makeAtom(rp, rp.uuid));
	return atom;
}

SQLAtomStorage::PseudoPtr SQLAtomStorage::petAtom(UUID uuid)
{
	setup_typemap();
	char buff[BUFSZ];
	snprintf(buff, BUFSZ, "SELECT * FROM Atoms WHERE uuid = %lu;", uuid);

	return getAtom(buff, -1);
}

/// Get the full outgoing set, recursively.
/// When adding links of unknown provenance, it could happen that
/// the outgoing set of the link has not yet been loaded.  In
/// that case, we have to load the outgoing set first.
///
/// Note that this does NOT fetch any values!
Handle SQLAtomStorage::get_recursive_if_not_exists(PseudoPtr p)
{
	if (nameserver().isA(p->type, NODE))
	{
		Handle h(_tlbuf.getAtom(p->uuid));
		if (h) return h;

		Handle node(createNode(p->type, p->name));
		_tlbuf.addAtom(node, p->uuid);
		_num_rec_nodes ++;
		return node;
	}
	HandleSeq resolved_oset;
	for (UUID idu : p->oset)
	{
		Handle h(_tlbuf.getAtom(idu));
		if (h)
		{
			resolved_oset.emplace_back(h);
			continue;
		}
		PseudoPtr po(petAtom(idu));

		// Corrupted databases can have outoging sets that refer
		// to non-existent atoms. This is rare, but has happened.
		// WIthout this check, the null-pointer deref will crash.
		if (nullptr == po)
			throw IOException(TRACE_INFO,
				"SQLAtomStorage::get_recursive_if_not_exists: "
				"Corrupt database; no atom for uuid=%lu", idu);

		Handle ha(get_recursive_if_not_exists(po));
		resolved_oset.emplace_back(ha);
	}
	Handle link(createLink(resolved_oset, p->type));
	_tlbuf.addAtom(link, p->uuid);
	_num_rec_links++;
	return link;
}

/**
 * Fetch the Node with the indicated type and name.
 * If there is no such node, NULL is returned.
 */
Handle SQLAtomStorage::doGetNode(Type t, const char * str)
{
	// First, check to see if we already know this Node.
	Handle node(createNode(t, str));
	UUID uuid = _tlbuf.getUUID(node);
	if (TLB::INVALID_UUID != uuid)
		return _tlbuf.getAtom(uuid);

	// If we don't know it, then go get it's UUID.
	setup_typemap();
	char buff[4*BUFSZ];

	// Use postgres $-quoting to make unicode strings easier to deal with.
	int nc = snprintf(buff, 4*BUFSZ, "SELECT * FROM Atoms WHERE "
		"type = %d AND name = $ocp$%s$ocp$ ;", storing_typemap[t], str);

	if (4*BUFSZ-1 <= nc)
	{
		buff[4*BUFSZ-1] = 0x0;
		throw IOException(TRACE_INFO,
			"SQLAtomStorage::getNode: buffer overflow!\n"
			"\tnc=%d buffer=>>%s<<\n", nc, buff);
		return Handle();
	}
#ifdef STORAGE_DEBUG
	_num_get_nodes++;
#endif // STORAGE_DEBUG

	PseudoPtr p(getAtom(buff, 0));
	if (NULL == p) return Handle();

#ifdef STORAGE_DEBUG
	_num_got_nodes++;
#endif // STORAGE_DEBUG
	_tlbuf.addAtom(node, p->uuid);
	return _tlbuf.getAtom(p->uuid);
}

Handle SQLAtomStorage::getNode(Type t, const char * str)
{
	Handle h(doGetNode(t, str));
	if (h) get_atom_values(h);
	return h;
}

/**
 * Fetch the Link with given type and outgoing set.
 * If there is no such link, NULL is returned.
 */
Handle SQLAtomStorage::doGetLink(Type t, const HandleSeq& hseq)
{
	// First, check to see if we already know this Link.
	Handle link(createLink(hseq, t));
	UUID uuid = _tlbuf.getUUID(link);
	if (TLB::INVALID_UUID != uuid)
		return _tlbuf.getAtom(uuid);

	// If the outgoing set is not yet known, then the link
	// itself cannot possibly be known.
	std::string ostr;
	try
	{
		ostr = oset_to_string(hseq);
	}
	catch (const NotFoundException& ex)
	{
		return Handle();
	}

	// If we don't know it, then go get it's UUID.
	setup_typemap();

	char buff[BUFSZ];
	snprintf(buff, BUFSZ,
		"SELECT * FROM Atoms WHERE type = %d AND outgoing = ",
		storing_typemap[t]);

	std::string qstr = buff;
	qstr += ostr;
	qstr += ";";

#ifdef STORAGE_DEBUG
	_num_get_links++;
#endif // STORAGE_DEBUG
	PseudoPtr p = getAtom(qstr.c_str(), 1);
	if (nullptr == p) return Handle();

#ifdef STORAGE_DEBUG
	_num_got_links++;
#endif // STORAGE_DEBUG
	_tlbuf.addAtom(link, p->uuid);
	return _tlbuf.getAtom(p->uuid);
}

Handle SQLAtomStorage::getLink(Type t, const HandleSeq& hs)
{
	Handle hg(doGetLink(t, hs));
	if (hg) get_atom_values(hg);
	return hg;
}

/**
 * Instantiate a new atom, from the response buffer contents
 */
SQLAtomStorage::PseudoPtr SQLAtomStorage::makeAtom(Response &rp, UUID uuid)
{
	// Now that we know everything about an atom, actually construct one.
	Type realtype = loading_typemap[rp.itype];

	if (NOTYPE == realtype)
	{
		throw IOException(TRACE_INFO,
			"Fatal Error: OpenCog does not have a type called %s\n",
			db_typename[rp.itype]);
		return NULL;
	}

	PseudoPtr atom(createPseudo());

	// All height zero atoms are nodes,
	// All positive height atoms are links.
	// A negative height is "unknown" and must be checked.
	if ((0 == rp.height) or
		((-1 == rp.height) and nameserver().isA(realtype, NODE)))
	{
		atom->name = rp.name;
	}
	else
	{
		char *p = (char *) rp.outlist;
		while (p)
		{
			// Break if there are no more atoms in the outgoing set
			// or if the outgoing set is empty in the first place.
			if (*p == '}' or *p == '\0') break;
			UUID out(strtoul(p+1, &p, 10));
			atom->oset.emplace_back(out);
		}
	}

	// Give the atom the correct UUID. The AtomTable will need this.
	atom->type = realtype;
	atom->uuid = uuid;

	_load_count ++;
	if (bulk_load and _load_count%100000 == 0)
	{
		time_t secs = time(0) - bulk_start;
		double rate = ((double) _load_count) / secs;
		unsigned long kays = ((unsigned long) _load_count) / 1000;
		printf("\tLoaded %luK atoms in %d seconds (%d per second).\n",
			kays, (int) secs, (int) rate);
	}

	return atom;
}

/* ============================= END OF FILE ================= */
