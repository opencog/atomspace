/*
 * opencog/persist/sql/multi-driver/SQLBulk.cc
 * Bulk save & restore of Atoms.
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
#include <time.h>
#include <unistd.h>

#include <chrono>
#include <memory>

#define OC_OMP 1  // hack alert -- force over-ride!
#include <opencog/util/oc_assert.h>
#include <opencog/util/oc_omp.h>

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
/**
 * Retreive the incoming set of the indicated atom.
 */
void SQLAtomStorage::getIncoming(AtomSpace& table, const char *buff)
{
	std::vector<PseudoPtr> pset;
	Response rp(conn_pool);
	rp.store = this;
	rp.height = -1;
	rp.pvec = &pset;
	rp.exec(buff);
	rp.rs->foreach_row(&Response::fetch_incoming_set_cb, &rp);

	HandleSeq iset;
	std::mutex iset_mutex;

	// Parallelize always.
	opencog::setting_omp(NUM_OMP_THREADS, NUM_OMP_THREADS);

	// A parallel fetch is much much faster, esp for big osets.
	// std::for_each(std::execution::par_unseq, ... requires C++17
	OMP_ALGO::for_each(pset.begin(), pset.end(),
		[&] (const PseudoPtr& p)
	{
		Handle hi(get_recursive_if_not_exists(p));
		hi = table.storage_add_nocheck(hi);
		_tlbuf.addAtom(hi, p->uuid);
		get_atom_values(hi);
		std::lock_guard<std::mutex> lck(iset_mutex);
		iset.emplace_back(hi);
	});

	// Performance stats
	_num_get_insets++;
	_num_get_inlinks += iset.size();
}

/**
 * Retreive the entire incoming set of the indicated atom.
 */
void SQLAtomStorage::getIncomingSet(AtomSpace& table, const Handle& h)
{
	rethrow();

	// If the uuid is not known, then the atom is not in storage,
	// and therefore, cannot have an incoming set.  Just return.
	UUID uuid = check_uuid(h);
	if (TLB::INVALID_UUID == uuid) return;

	char buff[BUFSZ];
	snprintf(buff, BUFSZ,
		"SELECT * FROM Atoms WHERE outgoing @> ARRAY[CAST(%lu AS BIGINT)];",
		uuid);

	// Note: "select * from atoms where outgoing@>array[556];" will
	// return all links with atom 556 in the outgoing set -- i.e. the
	// incoming set of 556.  We could also use && here instead of @>
	// but I don't know if this one is faster.
	// The cast to BIGINT is needed, as otherwise one gets
	// ERROR:  operator does not exist: bigint[] @> integer[]

	getIncoming(table, buff);
}

/**
 * Retreive the incoming set of the indicated atom, but only those atoms
 * of type t.
 */
void SQLAtomStorage::getIncomingByType(AtomSpace& table, const Handle& h, Type t)
{
	rethrow();

	// If the uuid is not known, then the atom is not in storage,
	// and therefore, cannot have an incoming set.  Just return.
	UUID uuid = check_uuid(h);
	if (TLB::INVALID_UUID == uuid) return;

	int dbtype = storing_typemap[t];

	char buff[BUFSZ];
	snprintf(buff, BUFSZ,
		"SELECT * FROM Atoms WHERE type = %d AND outgoing @> ARRAY[CAST(%lu AS BIGINT)];",
		dbtype, uuid);

	getIncoming(table, buff);
}

/* ================================================================ */

int SQLAtomStorage::getMaxObservedHeight(void)
{
	Response rp(conn_pool);
	rp.intval = 0;
	rp.exec("SELECT height FROM Atoms ORDER BY height DESC LIMIT 1;");
	rp.rs->foreach_row(&Response::intval_cb, &rp);
	return rp.intval;
}

void SQLAtomStorage::loadAtomSpace(AtomSpace &table)
{
	rethrow();
	UUID max_nrec = getMaxObservedUUID();
	size_t start_count = _load_count;
	max_height = getMaxObservedHeight();
	printf("Loading all atoms; maxuuid=%lu max height=%d\n",
		max_nrec, max_height);
	bulk_load = true;
	bulk_start = time(0);

	setup_typemap();

#define NCHUNKS 300
#define MINSTEP 10123
	std::vector<unsigned long> steps;
	unsigned long stepsize = MINSTEP + max_nrec/NCHUNKS;
	for (unsigned long rec = 0; rec <= max_nrec; rec += stepsize)
		steps.push_back(rec);

	printf("Loading all atoms: "
		"Max Height is %d stepsize=%lu chunks=%zu\n",
		 max_height, stepsize, steps.size());

	// Parallelize always.
	opencog::setting_omp(NUM_OMP_THREADS, NUM_OMP_THREADS);

	for (int hei=0; hei<=max_height; hei++)
	{
		unsigned long cur = _load_count;

		OMP_ALGO::for_each(steps.begin(), steps.end(),
			[&](unsigned long rec)
		{
			Response rp(conn_pool);
			rp.table = &table;
			rp.store = this;
			char buff[BUFSZ];
			snprintf(buff, BUFSZ, "SELECT * FROM Atoms WHERE "
			         "height = %d AND uuid > %lu AND uuid <= %lu;",
			         hei, rec, rec+stepsize);
			rp.height = hei;
			rp.exec(buff);
			rp.rs->foreach_row(&Response::load_all_atoms_cb, &rp);
		});
		printf("Loaded %lu atoms at height %d\n", _load_count - cur, hei);
	}

	time_t secs = time(0) - bulk_start;
	double rate = ((double) _load_count) / secs;
	printf("Finished loading %zu atoms in total in %d seconds (%d per second)\n",
		(_load_count - start_count), (int) secs, (int) rate);
	bulk_load = false;

	// synchrnonize!
	table.barrier();
}

void SQLAtomStorage::loadType(AtomSpace &table, Type atom_type)
{
	rethrow();

	UUID max_nrec = getMaxObservedUUID();
	size_t start_count = _load_count;

	// For links, assume a worst-case height.
	// For nodes, its easy ... max_height is zero.
	if (nameserver().isNode(atom_type))
		max_height = 0;
	else
		max_height = getMaxObservedHeight();

	setup_typemap();
	int db_atom_type = storing_typemap[atom_type];

#define NCHUNKS 300
#define MINSTEP 10123
	std::vector<unsigned long> steps;
	unsigned long stepsize = MINSTEP + max_nrec/NCHUNKS;
	for (unsigned long rec = 0; rec <= max_nrec; rec += stepsize)
		steps.push_back(rec);

	logger().debug("SQLAtomStorage::loadType: "
		"Max Height is %d stepsize=%lu chunks=%lu\n",
		 max_height, stepsize, steps.size());

	// Parallelize always.
	opencog::setting_omp(NUM_OMP_THREADS, NUM_OMP_THREADS);

	for (int hei=0; hei<=max_height; hei++)
	{
		unsigned long cur = _load_count;

		OMP_ALGO::for_each(steps.begin(), steps.end(),
			[&](unsigned long rec)
		{
			Response rp(conn_pool);
			rp.table = &table;
			rp.store = this;
			char buff[BUFSZ];
			snprintf(buff, BUFSZ, "SELECT * FROM Atoms WHERE type = %d "
			         "AND height = %d AND uuid > %lu AND uuid <= %lu;",
			         db_atom_type, hei, rec, rec+stepsize);
			rp.height = hei;
			rp.exec(buff);
			rp.rs->foreach_row(&Response::load_if_not_exists_cb, &rp);
		});
		logger().debug("SQLAtomStorage::loadType: "
		               "Loaded %lu atoms of type %d at height %d\n",
			_load_count - cur, db_atom_type, hei);
	}
	logger().debug("SQLAtomStorage::loadType: Finished loading %zu atoms in total\n",
		_load_count- start_count);

	// Synchronize!
	table.barrier();
}

/// Store all of the atoms in the atom table.
void SQLAtomStorage::storeAtomSpace(const AtomSpace &table)
{
	rethrow();

	max_height = 0;
	_store_count = 0;

#ifdef ALTER
	rename_tables();
	create_tables();
#endif

	UUID max_uuid = getMaxObservedUUID();
	logger().info("Bulk store to database with max UUID=%lu\n",
		 max_uuid);

	// If we are storing to an absolutely empty database, then
	// skip all UUID lookups completely!  This is not a safe
	// operation for non-empty databases, but has a big performance
	// impact for clean stores.
	// uuid==1 is PredicateNode TruthValueKey
	// uuid==2 is unissued.
	if (2 >= max_uuid) bulk_store = true;

	setup_typemap();
	store_atomtable_id(table);

	bulk_start = time(0);

	// Try to knock out the nodes first, then the links.
	HandleSeq atoms;
	atoms.reserve(table.get_num_nodes());
	table.get_handles_by_type(atoms, NODE, true);
	for (const Handle& h: atoms) { storeAtom(h); }

	atoms.clear();
	atoms.reserve(table.get_num_links());
	table.get_handles_by_type(atoms, LINK, true);
	for (const Handle& h: atoms) { storeAtom(h); }

	flushStoreQueue();
	bulk_store = false;

	time_t secs = time(0) - bulk_start;
	double rate = ((double) _store_count) / secs;
	printf("\tFinished storing %lu atoms total, in %d seconds (%d per second)\n",
		(unsigned long) _store_count, (int) secs, (int) rate);
}

/* ============================= END OF FILE ================= */
