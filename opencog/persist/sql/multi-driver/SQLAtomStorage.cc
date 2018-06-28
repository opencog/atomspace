/*
 * FUNCTION:
 * Persistent Atom storage, SQL-backed.
 *
 * Atoms and Values are saved to, and restored from, an SQL DB using
 * one of the available database drivers. Currently, the postgres
 * native libpq-dev API and the ODBC API are supported. Note that
 * libpq-dev is about three times faster than ODBC.
 *
 * Atoms are identified by means of unique ID's (UUID's), which are
 * correlated with specific in-RAM atoms via the TLB.
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
#include <thread>

#include <opencog/util/oc_assert.h>
#include <opencog/util/oc_omp.h>

#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/proto/NameServer.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atomspaceutils/TLB.h>

#include "SQLAtomStorage.h"
#include "SQLResponse.h"

#include "ll-pg-cxx.h"
#include "odbcxx.h"


using namespace opencog;

/* ================================================================ */
// Constructors

#define STORAGE_DEBUG 1

void SQLAtomStorage::init(const char * uri)
{
	_uri = uri;

	bool use_libpq = (0 == strncmp(uri, "postgres", 8));
	bool use_odbc = (0 == strncmp(uri, "odbc", 4));

	// default to postgres, if no driver given.
	if (uri[0] == '/') use_libpq = true;

	if (not use_libpq and not use_odbc)
		throw IOException(TRACE_INFO, "Unknown URI '%s'\n", uri);

	// Allow for one connection per database-reader, and one connection
	// for each writer.  Make sure that there are more connections than
	// there are writers, else both readers and writers starve.
	// Well, except that's not right.
	//
	// Hmm. This could be refined. On high core-count machines, this
	// opens a LOT of connections to the sql server, which hogs
	// resources.  PSQL does not like this, and complains. Most of the
	// time, all but a small handful of those connections are idle.
	// Except during loading, when approx 100% of the pool gets used,
	// due to the OMP_ALGO loops below. Likewise, when saving, when
	// just about 100% of the NUM_WB_QUEUES are full and busy.
	// So basically, the optimal solution seems to be to just set both
	// to be equal to the total number of cores.
	//
	// Except this doesn't work, for several reasons:
	// 1) The pool size has to be at least 1 larger than the OMP_ALGO,
	// otherwise, we'll deadlock. The problem is that during fetches,
	// two connections get used per OMP_ALGO thread.
	// 2) Postgres has efficiency problems scaling above 8 or 12
	// connections, at least, as of postgres 9.5 (2016)
	// actually, it seems not to be able to service more than 3 or 4
	// concurrent SELECT statements to the same table...
	// So, ignore the number of cores, and set things to 12.
	//
	// _initial_conn_pool_size = std::thread::hardware_concurrency();
	// if (0 == _initial_conn_pool_size) _initial_conn_pool_size = 8;
	// _initial_conn_pool_size += NUM_WB_QUEUES;
// #define NUM_OMP_THREADS 8

	// A large number of write-back queues do not seem to help
	// performance, because once the queues get above the high-water
	// mark, they *all* stall, waiting for the drain to complete.
	// Based on observations, postgres seems to have trouble servicing
	// more than 2 or 3 concurrent write requests on the same table,
	// and so anything above about 4 write-back quees seems to serve no
	// purpose. (Above statements for a 24-core CPU.)
#define NUM_WB_QUEUES 6

	_initial_conn_pool_size = NUM_OMP_THREADS + NUM_WB_QUEUES;
	for (int i=0; i<_initial_conn_pool_size; i++)
	{
		LLConnection* db_conn = nullptr;
#ifdef HAVE_PGSQL_STORAGE
		if (use_libpq)
			db_conn = new LLPGConnection(uri);
#endif /* HAVE_PGSQL_STORAGE */

#ifdef HAVE_ODBC_STORAGE
		if (use_odbc)
			db_conn = new ODBCConnection(uri);
#endif /* HAVE_ODBC_STORAGE */

		conn_pool.push(db_conn);
	}
	type_map_was_loaded = false;

	max_height = 0;
	bulk_load = false;
	bulk_store = false;
	clear_stats();

	for (int i=0; i< TYPEMAP_SZ; i++)
	{
		db_typename[i] = NULL;
	}

	if (!connected()) return;

	reserve();
	_next_valid = getMaxObservedVUID() + 1;

	// Special-case for TruthValues
	tvpred = doGetNode(PREDICATE_NODE, "*-TruthValueKey-*");
	if (nullptr == tvpred)
	{
		tvpred = createNode(PREDICATE_NODE, "*-TruthValueKey-*");
		do_store_single_atom(tvpred, 0);
	}

	// Special case for the pre-defined atomspaces.
	table_id_cache.insert(1);
}

SQLAtomStorage::SQLAtomStorage(std::string uri)
	: _write_queue(this, &SQLAtomStorage::vdo_store_atom, NUM_WB_QUEUES)
{
	init(uri.c_str());

	// Use a bigger buffer than the default. Assuming that the hardware
	// can do 1K atom stores/sec or better, this gives a backlog of
	// unwritten stuff less than a second long, which seems like an OK
	// situation, to me.
	_write_queue.set_watermarks(800, 150);
}

SQLAtomStorage::~SQLAtomStorage()
{
	flushStoreQueue();

	while (not conn_pool.is_empty())
	{
		LLConnection* db_conn = conn_pool.pop();
		delete db_conn;
	}

	for (int i=0; i<TYPEMAP_SZ; i++)
	{
		if (db_typename[i]) free(db_typename[i]);
	}
}

/**
 * connected -- return true if a successful connection to the
 * database exists; else return false.  Note that this may block,
 * if all database connections are in use...
 */
bool SQLAtomStorage::connected(void)
{
	// This will leak a resource, if db_conn->connected() ever throws.
	LLConnection* db_conn = conn_pool.pop();
	bool have_connection = db_conn->connected();
	conn_pool.push(db_conn);
	return have_connection;
}

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
/* AtomTable UUID stuff */
void SQLAtomStorage::store_atomtable_id(const AtomTable& at)
{
	UUID tab_id = at.get_uuid();
	if (table_id_cache.count(tab_id)) return;

	table_id_cache.insert(tab_id);

	// Get the parent table as well.
	UUID parent_id = 1;
	AtomTable *env = at.get_environ();
	if (env)
	{
		parent_id = env->get_uuid();
		store_atomtable_id(*env);
	}

#define BUFSZ 80
	char buff[BUFSZ];
	snprintf(buff, BUFSZ,
		"INSERT INTO Spaces (space, parent) VALUES (%ld, %ld);",
		tab_id, parent_id);

	Response rp(conn_pool);
	rp.exec(buff);
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

/// Drain the pending store queue. This is a fencing operation; the
/// goal is to make sure that all writes that occurred before the
/// barrier really are performed before before all the writes after
/// the barrier.
///
/// Caution: this is potentially racey in two different ways.
/// First, there is a small window in the async_caller implementation,
/// where, if the timing is just so, the barrier might return before
/// the last element is written.  (Although everything else will have
/// gone out; only the last element is in doubt). Technically, that's
/// a bug, but its sufficiently "minor" so we don't fix it.
///
/// The second issue is more serious: there's no fence or barrier in
/// Postgres (that I can find or think of), and so although we've sent
/// everything to PG, there's no guarantee that PG will process these
/// requests in order. How likely this could be, I don't know.
///
void SQLAtomStorage::flushStoreQueue()
{
	_write_queue.barrier();
}

/* ================================================================ */

void SQLAtomStorage::rename_tables(void)
{
	Response rp(conn_pool);

	rp.exec("ALTER TABLE Atoms RENAME TO Atoms_Backup;");
	rp.exec("ALTER TABLE Global RENAME TO Global_Backup;");
	rp.exec("ALTER TABLE TypeCodes RENAME TO TypeCodes_Backup;");
}

void SQLAtomStorage::create_tables(void)
{
	Response rp(conn_pool);

	// See the file "atom.sql" for detailed documentation as to the
	// structure of the SQL tables. The code below is kept in sync,
	// manually, with the contents of atom.sql.
	rp.exec("CREATE TABLE Spaces ("
	              "space     BIGINT PRIMARY KEY,"
	              "parent    BIGINT);");

	rp.exec("INSERT INTO Spaces VALUES (0,0);");
	rp.exec("INSERT INTO Spaces VALUES (1,1);");

	rp.exec("CREATE TABLE Atoms ("
	            "uuid     BIGINT PRIMARY KEY,"
	            "space    BIGINT REFERENCES spaces(space),"
	            "type     SMALLINT,"
	            "height   SMALLINT,"
	            "name     TEXT,"
	            "outgoing BIGINT[],"
	            "UNIQUE (type, name),"
	            "UNIQUE (type, outgoing));");

	rp.exec("CREATE TABLE Valuations ("
	            "key BIGINT REFERENCES Atoms(uuid),"
	            "atom BIGINT REFERENCES Atoms(uuid),"
	            "type  SMALLINT,"
	            "floatvalue DOUBLE PRECISION[],"
	            "stringvalue TEXT[],"
	            "linkvalue BIGINT[],"
	            "UNIQUE (key, atom));");

	rp.exec("CREATE INDEX ON Valuations (atom);");

	rp.exec("CREATE TABLE Values ("
	            "vuid BIGINT PRIMARY KEY,"
	            "type  SMALLINT,"
	            "floatvalue DOUBLE PRECISION[],"
	            "stringvalue TEXT[],"
	            "linkvalue BIGINT[]);");

	rp.exec("CREATE TABLE TypeCodes ("
	            "type SMALLINT UNIQUE,"
	            "typename TEXT UNIQUE);");

	type_map_was_loaded = false;
}

/**
 * kill_data -- destroy data in the database!! Dangerous !!
 * This routine is meant to be used only for running test cases.
 * It is extremely dangerous, as it can lead to total data loss.
 */
void SQLAtomStorage::kill_data(void)
{
	Response rp(conn_pool);

	// See the file "atom.sql" for detailed documentation as to the
	// structure of the SQL tables.
	rp.exec("DELETE from Valuations;");
	rp.exec("DELETE from Values;");
	rp.exec("DELETE from Atoms;");

	// Delete the atomspaces as well!
	rp.exec("DELETE from Spaces;");

	rp.exec("INSERT INTO Spaces VALUES (0,0);");
	rp.exec("INSERT INTO Spaces VALUES (1,1);");

	// Special case for TruthValues - must always have this atom.
	_tlbuf.clear();
	do_store_single_atom(tvpred, 0);
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

/* ================================================================ */

void SQLAtomStorage::set_hilo_watermarks(int hi, int lo)
{
	_write_queue.set_watermarks(hi, lo);
}

void SQLAtomStorage::set_stall_writers(bool stall)
{
	_write_queue.stall(stall);
}

void SQLAtomStorage::clear_stats(void)
{
	_stats_time = time(0);
	_load_count = 0;
	_store_count = 0;
	_valuation_stores = 0;
	_value_stores = 0;

	_write_queue.clear_stats();

#ifdef STORAGE_DEBUG
	_num_get_nodes = 0;
	_num_got_nodes = 0;
	_num_rec_nodes = 0;
	_num_get_links = 0;
	_num_got_links = 0;
	_num_rec_links = 0;
	_num_get_insets = 0;
	_num_get_inlinks = 0;
	_num_node_inserts = 0;
	_num_link_inserts = 0;
	_num_atom_removes = 0;
	_num_atom_deletes = 0;
#endif // STORAGE_DEBUG
}

void SQLAtomStorage::print_stats(void)
{
	printf("sql-stats: Currently open URI: %s\n", _uri.c_str());
	time_t now = time(0);
	// ctime returns string with newline at end of it.
	printf("sql-stats: Time since stats reset=%lu secs, at %s",
		now - _stats_time, ctime(&_stats_time));


	size_t load_count = _load_count;
	size_t store_count = _store_count;
	double frac = store_count / ((double) load_count);
	printf("sql-stats: total loads = %zu total stores = %zu ratio=%f\n",
	       load_count, store_count, frac);

	size_t valuation_stores = _valuation_stores;
	size_t value_stores = _value_stores;
	printf("sql-stats: valuation updates = %zu value updates = %zu\n",
	       valuation_stores, value_stores);

	size_t num_atom_removes = _num_atom_removes;
	size_t num_atom_deletes = _num_atom_deletes;
	printf("sql-stats: atom remove requests = %zu total atom deletes = %zu\n",
	       num_atom_removes, num_atom_deletes);
	printf("\n");

#ifdef STORAGE_DEBUG
	size_t num_get_nodes = _num_get_nodes;
	size_t num_got_nodes = _num_got_nodes;
	size_t num_rec_nodes = _num_rec_nodes;
	size_t num_get_links = _num_get_links;
	size_t num_got_links = _num_got_links;
	size_t num_rec_links = _num_rec_links;
	size_t num_get_insets = _num_get_insets;
	size_t num_get_inlinks = _num_get_inlinks;
	size_t num_node_inserts = _num_node_inserts;
	size_t num_link_inserts = _num_link_inserts;

	frac = 100.0 * num_got_nodes / ((double) num_get_nodes);
	printf("num_get_nodes=%zu num_got_nodes=%zu (%f pct) recursive=%zu\n",
	       num_get_nodes, num_got_nodes, frac, num_rec_nodes);

	frac = 100.0 * num_got_links / ((double) num_get_links);
	printf("num_get_links=%zu num_got_links=%zu (%f pct) recursive=%zu\n",
	       num_get_links, num_got_links, frac, num_rec_links);

	frac = num_get_inlinks / ((double) num_get_insets);
	printf("num_get_incoming_sets=%zu set total=%zu avg set size=%f\n",
	       num_get_insets, num_get_inlinks, frac);

	unsigned long tot_node = num_node_inserts;
	unsigned long tot_link = num_link_inserts;
	frac = tot_link / ((double) tot_node);
	printf("total stores for node=%lu link=%lu ratio=%f\n",
	       tot_node, tot_link, frac);
#endif // STORAGE_DEBUG

	// Store queue performance
	unsigned long item_count = _write_queue._item_count;
	unsigned long duplicate_count = _write_queue._duplicate_count;
	unsigned long flush_count = _write_queue._flush_count;
	unsigned long drain_count = _write_queue._drain_count;
	unsigned long drain_msec = _write_queue._drain_msec;
	unsigned long drain_slowest_msec = _write_queue._drain_slowest_msec;
	unsigned long drain_concurrent = _write_queue._drain_concurrent;
	int high_water = _write_queue.get_high_watermark();
	int low_water = _write_queue.get_low_watermark();
	bool stalling = _write_queue.stalling();

	double dupe_frac = duplicate_count / ((double) (item_count - duplicate_count));
	double flush_frac = (item_count - duplicate_count) / ((double) flush_count);
	double fill_frac = (item_count - duplicate_count) / ((double) drain_count);

	unsigned long dentries = drain_count + drain_concurrent;
	double drain_ratio = dentries / ((double) drain_count);
	double drain_secs = 0.001 * drain_msec / ((double) dentries);
	double slowest = 0.001 * drain_slowest_msec;

	printf("\n");
	printf("hi-water=%d low-water=%d stalling=%s\n", high_water,
	       low_water, stalling? "true" : "false");
	printf("write items=%lu dup=%lu dupe_frac=%f flushes=%lu flush_ratio=%f\n",
	       item_count, duplicate_count, dupe_frac, flush_count, flush_frac);
	printf("drains=%lu fill_fraction=%f concurrency=%f\n",
	       drain_count, fill_frac, drain_ratio);
	printf("avg drain time=%f seconds; longest drain time=%f\n",
	       drain_secs, slowest);

	printf("currently in_drain=%d num_busy=%lu queue_size=%lu\n",
	       _write_queue._in_drain, _write_queue.get_busy_writers(),
	       _write_queue.get_size());

	printf("current conn_pool free=%u of %d\n", conn_pool.size(),
	       _initial_conn_pool_size);

	// Some basic TLB statistics; could be improved;
	// The TLB remapping theory needs some work...
	// size_t noh = 0;
	// size_t remap = 0;

	UUID mad = _tlbuf.getMaxUUID();
#if DONT_COUNT
	This loop can lead to an apparent hang, when max UUID gets
	// above a quarter-billion or so.  So don't do this.
	for (UUID uuid = 1; uuid < mad; uuid++)
	{
		Handle h = _tlbuf.getAtom(uuid);
		if (nullptr == h) { noh++; continue; }

#if 0
		Handle hr = as->get_atom(h);
		if (nullptr == hr) { extra++; continue; }
		if (hr != h) { remap++; }
#endif
	}
#endif

	printf("\n");
	printf("sql-stats: tlbuf holds %lu atoms\n", _tlbuf.size());
#if 0
	frac = 100.0 * extra / ((double) _tlbuf.size());
	printf("sql-stats: tlbuf holds %lu atoms not in atomspace (%f pct)\n",
	        extra, frac);

	frac = 100.0 * remap / ((double) _tlbuf.size());
	printf("sql-stats: tlbuf holds %lu unremapped atoms (%f pct)\n",
	       remap, frac);
#endif

	mad -= 1;
	size_t used = _tlbuf.size();
	frac = 100.0 * used / ((double) mad);
	printf("sql-stats: %zu of %lu reserved uuids used (%f pct)\n",
	       used, mad, frac);
}

/* ============================= END OF FILE ================= */
