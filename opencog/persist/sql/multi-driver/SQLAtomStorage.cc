/*
 * SQLAtomStorage.cc
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
 * Copyright (c) 2008,2009,2013,2015,2017 Linas Vepstas <linas@linas.org>
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

#include <opencog/atoms/atom_types/types.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atomspaceutils/TLB.h>
#include <opencog/persist/storage/storage_types.h>

#include "SQLAtomStorage.h"
#include "SQLResponse.h"

#include "ll-pg-cxx.h"
#include "odbcxx.h"

using namespace opencog;

// A large number of write-back queues do not seem to help
// performance, because once the queues get above the high-water
// mark, they *all* stall, waiting for the drain to complete.
// Based on observations, postgres seems to have trouble servicing
// more than 2 or 3 concurrent write requests on the same table,
// and so anything above about 4 write-back quees seems to serve no
// purpose. (Above statements for a 24-core CPU.)
#define NUM_WB_QUEUES 6

/* ================================================================ */
// Constructors

SQLAtomStorage::SQLAtomStorage(const std::string uri) :
	StorageNode(POSTGRES_STORAGE_NODE, uri),
	_tlbuf(&_uuid_manager),
	_uuid_manager("uuid_pool"),
	_vuid_manager("vuid_pool"),
	_write_queue(this, &SQLAtomStorage::vdo_store_atom, NUM_WB_QUEUES),
	_async_write_queue_exception(nullptr)
{
	// Use a bigger buffer than the default. Assuming that the hardware
	// can do 1K atom stores/sec or better, this gives a backlog of
	// unwritten stuff less than a second long, which seems like an OK
	// situation, to me.
	_write_queue.set_watermarks(800, 150);

	_initial_conn_pool_size = 0;
	_use_libpq = false;
	_use_odbc = false;

	type_map_was_loaded = false;
	for (int i=0; i< TYPEMAP_SZ; i++)
		db_typename[i] = NULL;

	max_height = 0;
	bulk_load = false;
	bulk_store = false;
	clear_stats();
}

SQLAtomStorage::~SQLAtomStorage()
{
	close_conn_pool();

	for (int i=0; i<TYPEMAP_SZ; i++)
	{
		if (db_typename[i]) free(db_typename[i]);
	}
}

/* ================================================================ */
// Connections and opening

void SQLAtomStorage::enlarge_conn_pool(int delta)
{
	if (0 >= delta) return;

	const char * uri = _name.c_str();
	for (int i=0; i<delta; i++)
	{
		LLConnection* db_conn = nullptr;
#ifdef HAVE_PGSQL_STORAGE
		if (_use_libpq)
			db_conn = new LLPGConnection(uri);
#endif /* HAVE_PGSQL_STORAGE */

#ifdef HAVE_ODBC_STORAGE
		if (_use_odbc)
			db_conn = new ODBCConnection(uri);
#endif /* HAVE_ODBC_STORAGE */

		conn_pool.push(db_conn);
	}

	_initial_conn_pool_size += delta;
}

void SQLAtomStorage::close_conn_pool()
{
	flushStoreQueue();

	while (not conn_pool.is_empty())
	{
		LLConnection* db_conn = conn_pool.value_pop();
		delete db_conn;
	}
	_initial_conn_pool_size = 0;
}

// Public function
void SQLAtomStorage::connect(void)
{
	connect(_name.c_str());
}

// Private internal-use-only
void SQLAtomStorage::connect(const char * uri)
{
	_use_libpq = (0 == strncmp(uri, "postgres", 8));
	_use_odbc = (0 == strncmp(uri, "odbc", 4));

	// Default to postgres, if no driver given.
	if (uri[0] == '/') _use_libpq = true;

	if (not _use_libpq and not _use_odbc)
		throw IOException(TRACE_INFO, "Unknown URI '%s'\n", uri);

	if (0 == _initial_conn_pool_size)
		enlarge_conn_pool(NUM_WB_QUEUES + 2);

	if (!connected()) return;

	// Need the server version before init'ing the UUID pool.
	get_server_version();
}

void SQLAtomStorage::open(void)
{
	connect();

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
	// connections, at least, as of postgres 9.5 (2016).
	// Actually, it seems not to be able to service more than 3 or 4
	// concurrent SELECT statements to the same table...
	// So, ignore the number of cores, and set things to 12.
	//
	// _initial_conn_pool_size = std::thread::hardware_concurrency();
	// if (0 == _initial_conn_pool_size) _initial_conn_pool_size = 8;
	// _initial_conn_pool_size += NUM_WB_QUEUES;
// #define NUM_OMP_THREADS 8

	// minus 2 because we had a +2 in connect();
	enlarge_conn_pool(NUM_OMP_THREADS - 2);

	if (!connected()) return;

	_uuid_manager.that = this;
	_uuid_manager.reset_uuid_pool(getMaxObservedUUID());
	_vuid_manager.that = this;
	_vuid_manager.reset_uuid_pool(getMaxObservedVUID());

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

/**
 * connected -- return true if a successful connection to the
 * database exists; else return false.  Note that this may block,
 * if all database connections are in use...
 */
bool SQLAtomStorage::connected(void)
{
	if (0 == _initial_conn_pool_size) return false;

	// This will leak a resource, if db_conn->connected() ever throws.
	LLConnection* db_conn = conn_pool.value_pop();
	bool have_connection = db_conn->connected();
	conn_pool.push(db_conn);
	return have_connection;
}

/** get_server_version() -- get version of postgres server */
void SQLAtomStorage::get_server_version(void)
{
	Response rp(conn_pool);
	rp.exec("SHOW server_version_num;");
	rp.rs->foreach_row(&Response::intval_cb, &rp);
	_server_version = rp.intval;
}

/// Rethrow asynchronous exceptions caught during atom storage.
///
/// Atoms are stored asynchronously, from a write queue, from some
/// other thread. If that thread has an exception, e.g. due to some
/// SQL error, and the exception is uncaught, then the process will
/// die. So we have to catch that exception.  Once caught, what do
/// we do with it? Well, we could ignore it, but then the user would
/// not know that the SQL backend was damaged. So, instead, we throw
/// it at the first user, any user that is doing some other SQL stuff.
void SQLAtomStorage::rethrow(void)
{
	if (_async_write_queue_exception)
	{
		std::exception_ptr exptr = _async_write_queue_exception;
		_async_write_queue_exception = nullptr;
		std::rethrow_exception(exptr);
	}
}

/* ================================================================== */
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
	rethrow();
	_write_queue.barrier();
	rethrow();
}

void SQLAtomStorage::barrier()
{
	flushStoreQueue();
}

/* ================================================================ */

void SQLAtomStorage::rename_tables(void)
{
	Response rp(conn_pool);

	rp.exec("ALTER TABLE Atoms RENAME TO Atoms_Backup;");
	rp.exec("ALTER TABLE Global RENAME TO Global_Backup;");
	rp.exec("ALTER TABLE TypeCodes RENAME TO TypeCodes_Backup;");
}

void SQLAtomStorage::create_database(void)
{
	const std::string& uri(_name);

	// Parse the URI and make a valiant attempt to extract a
	// database name from it. This ignores any usernames or
	// passwords that might follow the database name.
	// If ou want to get fancier, then fix this.
	if (strncmp(uri.c_str(), "postgres://", 11) and
	    uri.npos != uri.find_first_of("?&"))
	{
		throw IOException(TRACE_INFO, "Unknown URI '%s'\n", uri.c_str());
	}

	std::string server(uri);
	std::string dbname(uri);

	size_t pos = uri.find_last_of('/');
	if (pos == uri.npos)
		throw IOException(TRACE_INFO, "Unsupported URI '%s'\n", uri.c_str());

	server = uri.substr(0, pos);
	dbname = uri.substr(pos+1);

	// We need a temporary, administrative connection, to create
	// the database.  Let's assume the user has admin access; if
	// not, then libpq will deliver an error.
	connect(server.c_str());
	if (!connected())
		throw IOException(TRACE_INFO, "Error: cannot connect to '%s'",
		                  server.c_str());

	{
		Response rp(conn_pool);
		rp.exec("CREATE DATABASE " + dbname + ";");
	}
	{
		Response rp(conn_pool);
		rp.exec("COMMENT ON DATABASE " + dbname +
			" IS 'OpenCog AtomSpace';");
	}
	close_conn_pool();

	// Now reconnect, and create the tables.
	connect();
	create_tables();
}

void SQLAtomStorage::create_tables(void)
{
	Response rp(conn_pool);

	// See the file `atom.sql` for detailed documentation as to the
	// structure of the SQL tables. The code below is kept in sync,
	// manually, with the contents of `atom.sql`.
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

	rp.exec("CREATE INDEX incoming_idx on Atoms USING GIN(outgoing);");

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

	rp.exec("CREATE SEQUENCE uuid_pool START WITH 1 INCREMENT BY 400;");
	rp.exec("CREATE SEQUENCE vuid_pool START WITH 1 INCREMENT BY 400;");

	type_map_was_loaded = false;
}

/**
 * kill_data -- destroy data in the database!! Dangerous !!
 * This routine is meant to be used only for running test cases.
 * It is extremely dangerous, as it can lead to total data loss.
 */
void SQLAtomStorage::kill_data(void)
{
	rethrow();
	Response rp(conn_pool);

	// See the file "atom.sql" for detailed documentation as to the
	// structure of the SQL tables.
	rp.exec("DELETE from Valuations;");
	rp.exec("DELETE from Values;");
	rp.exec("DELETE from Atoms;");
	rp.exec("DROP SEQUENCE uuid_pool;");
	rp.exec("DROP SEQUENCE vuid_pool;");
	rp.exec("CREATE SEQUENCE uuid_pool START WITH 1 INCREMENT BY 400;");
	rp.exec("CREATE SEQUENCE vuid_pool START WITH 1 INCREMENT BY 400;");

	// Delete the atomspaces as well!
	rp.exec("DELETE from Spaces;");

	rp.exec("INSERT INTO Spaces VALUES (0,0);");
	rp.exec("INSERT INTO Spaces VALUES (1,1);");

	// Special case for TruthValues - must always have this atom.
	_uuid_manager.reset_uuid_pool(0);
	_vuid_manager.reset_uuid_pool(0);
	_tlbuf.clear();
	do_store_single_atom(tvpred, 0);
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
}

void SQLAtomStorage::print_stats(void)
{
	printf("sql-stats: Currently open URI: %s\n", _name.c_str());
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

	UUID mad = getMaxObservedUUID();
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

	size_t used = _tlbuf.size();
	frac = 100.0 * used / ((double) mad);
	printf("sql-stats: %zu of %lu reserved uuids used (%f pct)\n",
	       used, mad, frac);
}

DEFINE_NODE_FACTORY(PostgresStorageNode, POSTGRES_STORAGE_NODE)

/* ============================= END OF FILE ================= */
