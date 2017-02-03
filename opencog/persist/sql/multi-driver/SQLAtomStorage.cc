/*
 * FUNCTION:
 * Persistent Atom storage, SQL-backed.
 *
 * Atoms are saved to, and restored from, an SQL DB using one of the
 * avaialble database drivers. Curently, postgres native libpq-dev and
 * ODBC are supported.
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
#ifdef HAVE_SQL_STORAGE

#include <stdlib.h>
#include <unistd.h>

#include <chrono>
#include <memory>
#include <thread>

#include <opencog/util/oc_assert.h>
#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/base/ClassServer.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/truthvalue/CountTruthValue.h>
#include <opencog/truthvalue/IndefiniteTruthValue.h>
#include <opencog/truthvalue/ProbabilisticTruthValue.h>
#include <opencog/truthvalue/SimpleTruthValue.h>
#include <opencog/truthvalue/TruthValue.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atomspaceutils/TLB.h>

#include "llapi.h"
#include "ll-pg-cxx.h"
#include "odbcxx.h"
#include "SQLAtomStorage.h"

using namespace opencog;

/* ================================================================ */

/**
 * Utility class, hangs on to a single response to an SQL query,
 * and provides routines to parse it, i.e. walk the rows and columns,
 * converting each row into an Atom, or Edge.
 *
 * Intended to be allocated on stack, to avoid malloc overhead.
 * Methods are intended to be inlined, so as to avoid subroutine
 * call overhead.  It really *is* supposed to be a convenience wrapper. :-)
 */
class SQLAtomStorage::Response
{
	public:
		LLRecordSet *rs;

		// Temporary cache of info about atom being assembled.
		UUID uuid;
		int itype;
		const char * name;
		int tv_type;
		double mean;
		double confidence;
		double count;
		const char *outlist;
		int height;

		Response()
		{
			tname = "";
			itype = 0;
			intval = 0;
		}

		void release()
		{
			if (rs) rs->release();
			rs = nullptr;
		}
		bool create_atom_column_cb(const char *colname, const char * colvalue)
		{
			// printf ("%s = %s\n", colname, colvalue);
			if (!strcmp(colname, "type"))
			{
				itype = atoi(colvalue);
			}
			else if (!strcmp(colname, "name"))
			{
				name = colvalue;
			}
			else if (!strcmp(colname, "outgoing"))
			{
				outlist = colvalue;
			}
			if (!strcmp(colname, "tv_type"))
			{
				tv_type = atoi(colvalue);
			}
			else if (!strcmp(colname, "stv_mean"))
			{
				mean = atof(colvalue);
			}
			else if (!strcmp(colname, "stv_confidence"))
			{
				confidence = atof(colvalue);
			}
			else if (!strcmp(colname, "stv_count"))
			{
				count = atof(colvalue);
			}
			else if (!strcmp(colname, "uuid"))
			{
				uuid = strtoul(colvalue, NULL, 10);
			}
			return false;
		}
		bool create_atom_cb(void)
		{
			// printf ("---- New atom found ----\n");
			rs->foreach_column(&Response::create_atom_column_cb, this);

			return false;
		}

		AtomTable *table;
		SQLAtomStorage *store;
		bool load_all_atoms_cb(void)
		{
			// printf ("---- New atom found ----\n");
			rs->foreach_column(&Response::create_atom_column_cb, this);

			PseudoPtr p(store->makeAtom(*this, uuid));
			AtomPtr atom(get_recursive_if_not_exists(p));
			Handle h = table->add(atom, false);

			// Force resolution in TLB, so that later removes work.
			store->_tlbuf.addAtom(h, TLB::INVALID_UUID);
			return false;
		}

		// Load an atom into the atom table, but only if it's not in
		// it already.  The goal is to avoid clobbering the truth value
		// that is currently in the AtomTable.  Adding an atom to the
		// atom table that already exists causes the two TV's to be
		// merged, which is probably not what was wanted...
		bool load_if_not_exists_cb(void)
		{
			// printf ("---- New atom found ----\n");
			rs->foreach_column(&Response::create_atom_column_cb, this);

			if (nullptr == store->_tlbuf.getAtom(uuid))
			{
				PseudoPtr p(store->makeAtom(*this, uuid));
				AtomPtr atom(get_recursive_if_not_exists(p));
				Handle h = table->getHandle(atom);
				if (nullptr == h)
				{
					h = table->add(atom, false);
					store->_tlbuf.addAtom(h, uuid);
				}
			}
			return false;
		}

		HandleSeq *hvec;
		bool fetch_incoming_set_cb(void)
		{
			// printf ("---- New atom found ----\n");
			rs->foreach_column(&Response::create_atom_column_cb, this);

			// Note, unlike the above 'load' routines, this merely fetches
			// the atoms, and returns a vector of them.  They are loaded
			// into the atomspace later, by the caller.
			PseudoPtr p(store->makeAtom(*this, uuid));
			AtomPtr atom(get_recursive_if_not_exists(p));
			hvec->emplace_back(atom->getHandle());
			return false;
		}

		// Helper function for above.  The problem is that, when
		// adding links of unknown provenance, it could happen that
		// the outgoing set of the link has not yet been loaded.  In
		// that case, we have to load the outgoing set first.
		AtomPtr get_recursive_if_not_exists(PseudoPtr p)
		{
			if (classserver().isA(p->type, NODE))
			{
				NodePtr node(createNode(p->type, p->name, p->tv));
				store->_tlbuf.addAtom(node, p->uuid);
				return node;
			}
			HandleSeq resolved_oset;
			for (UUID idu : p->oset)
			{
				Handle h = store->_tlbuf.getAtom(idu);
				if (h)
				{
					resolved_oset.emplace_back(h);
					continue;
				}
				PseudoPtr po(store->petAtom(idu));
				AtomPtr ra = get_recursive_if_not_exists(po);
				resolved_oset.emplace_back(ra->getHandle());
			}
			LinkPtr link(createLink(p->type, resolved_oset, p->tv));
			store->_tlbuf.addAtom(link, p->uuid);
			return link;
		}

		bool row_exists;
		bool row_exists_cb(void)
		{
			row_exists = true;
			return false;
		}

		// deal twith the type-to-id map
		bool type_cb(void)
		{
			rs->foreach_column(&Response::type_column_cb, this);
			store->set_typemap(itype, tname);
			return false;
		}
		const char * tname;
		bool type_column_cb(const char *colname, const char * colvalue)
		{
			if (!strcmp(colname, "type"))
			{
				itype = atoi(colvalue);
			}
			else if (!strcmp(colname, "typename"))
			{
				tname = colvalue;
			}
			return false;
		}
#ifdef OUT_OF_LINE_TVS
		// Callbacks for SimpleTruthValues
		int tvid;
		bool create_tv_cb(void)
		{
			// printf ("---- New SimpleTV found ----\n");
			rs->foreach_column(&Response::create_tv_column_cb, this);
			return false;
		}
		bool create_tv_column_cb(const char *colname, const char * colvalue)
		{
			printf ("%s = %s\n", colname, colvalue);
			if (!strcmp(colname, "mean"))
			{
				mean = atof(colvalue);
			}
			else if (!strcmp(colname, "count"))
			{
				count = atof(colvalue);
			}
			return false;
		}

#endif /* OUT_OF_LINE_TVS */

		// get generic positive integer values
		unsigned long intval;
		bool intval_cb(void)
		{
			rs->foreach_column(&Response::intval_column_cb, this);
			return false;
		}
		bool intval_column_cb(const char *colname, const char * colvalue)
		{
			// we're not going to bother to check the column name ...
			intval = strtoul(colvalue, NULL, 10);
			return false;
		}

		// Get all handles in the database.
		std::set<UUID> *id_set;
		bool note_id_cb(void)
		{
			rs->foreach_column(&Response::note_id_column_cb, this);
			return false;
		}
		bool note_id_column_cb(const char *colname, const char * colvalue)
		{
			// we're not going to bother to check the column name ...
			UUID id = strtoul(colvalue, NULL, 10);
			id_set->insert(id);
			return false;
		}
};

/* ================================================================ */
/// XXX TODO Make the connection pointer scoped.
/// That is, we should define a ConnPtr class here, and it's destructor
/// should do the conn_pool.push(). Doing this can help avoid mem leaks,
/// e.g. failure to put because of a throw.  I'm just kind of lazy now,
/// and the code below works ... so maybe it shouldn't be messed with.
///
/// XXX Should do the same for Response rp.rs->release() to auto-release.

/// Get an SQL connection.  If the pool is empty, this will block,
/// waiting for a connection to be returned to the pool. Thus, the
/// size of the pool regulates how many outstanding SQL requests can
/// be pending in parallel.
LLConnection* SQLAtomStorage::get_conn()
{
	return conn_pool.pop();
}

/// Put an SQL connection back into the pool.
void SQLAtomStorage::put_conn(LLConnection* db_conn)
{
	conn_pool.push(db_conn);
}

/* ================================================================ */

bool SQLAtomStorage::idExists(const char * buff)
{
	LLConnection* db_conn = get_conn();
	Response rp;
	rp.row_exists = false;
	rp.rs = db_conn->exec(buff);
	rp.rs->foreach_row(&Response::row_exists_cb, &rp);
	rp.release();
	put_conn(db_conn);
	return rp.row_exists;
}

/* ================================================================ */
// Constructors

void SQLAtomStorage::init(const char * uri)
{
	bool use_libpq = (0 == strncmp(uri, "postgres", 8));
	bool use_odbc = (0 == strncmp(uri, "odbc", 4));

	// default to postgres, if no driver given.
	if (uri[0] == '/') use_libpq = true;

	if (not use_libpq and not use_odbc)
		throw IOException(TRACE_INFO, "Unknown URI '%s'\n", uri);

	_initial_conn_pool_size = std::thread::hardware_concurrency();
	if (0 == _initial_conn_pool_size) _initial_conn_pool_size = 8;
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
	_load_count = 0;
	_store_count = 0;

	for (int i=0; i< TYPEMAP_SZ; i++)
	{
		db_typename[i] = NULL;
	}

	local_id_cache_is_inited = false;
	if (!connected()) return;

	reserve();

#define STORAGE_DEBUG 1
#ifdef STORAGE_DEBUG
	_num_get_nodes = 0;
	_num_got_nodes = 0;
	_num_get_links = 0;
	_num_got_links = 0;
	_num_get_insets = 0;
	_num_get_inatoms = 0;
	_num_node_updates = 0;
	_num_node_inserts = 0;
	_num_link_updates = 0;
	_num_link_inserts = 0;
#endif // STORAGE_DEBUG
}

// Initialize with 8 write-back queues, but maybe this should be...
// I dunno -- std::thread::hardware_concurrency()  ???
SQLAtomStorage::SQLAtomStorage(std::string uri)
	: _write_queue(this, &SQLAtomStorage::vdo_store_atom, 8)
{
	init(uri.c_str());
}

SQLAtomStorage::~SQLAtomStorage()
{
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
	LLConnection* db_conn = get_conn();
	bool have_connection = db_conn->connected();
	put_conn(db_conn);
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
	_extract_sig = as->removeAtomSignal(
		boost::bind(&SQLAtomStorage::extract_callback, this, _1));
#endif // NOT_NEEDED_RIGHT_NOW
}

void SQLAtomStorage::unregisterWith(AtomSpace* as)
{
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
#define BUFSZ 250

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

	char buff[BUFSZ];
	snprintf(buff, BUFSZ,
		"INSERT INTO Spaces (space, parent) VALUES (%ld, %ld);",
		tab_id, parent_id);

	std::unique_lock<std::mutex> lock(table_cache_mutex);
	LLConnection* db_conn = get_conn();
	Response rp;
	rp.rs = db_conn->exec(buff);
	rp.release();
	put_conn(db_conn);
}


/* ================================================================ */

#define STMT(colname,val) { \
	if (update) { \
		if (notfirst) { cols += ", "; } else notfirst = true; \
		cols += colname; \
		cols += " = "; \
		cols += val; \
	} else { \
		if (notfirst) { cols += ", "; vals += ", "; } else notfirst = true; \
		cols += colname; \
		vals += val; \
	} \
}

#define STMTI(colname,ival) { \
	char buff[BUFSZ]; \
	snprintf(buff, BUFSZ, "%u", ival); \
	STMT(colname, buff); \
}

#define STMTF(colname,fval) { \
	char buff[BUFSZ]; \
	snprintf(buff, BUFSZ, "%12.8g", fval); \
	STMT(colname, buff); \
}

/* ================================================================ */

#ifdef OUT_OF_LINE_TVS
/**
 * Return true if the indicated handle exists in the storage.
 */
bool SQLAtomStorage::tvExists(int tvid)
{
	char buff[BUFSZ];
	snprintf(buff, BUFSZ, "SELECT tvid FROM SimpleTVs WHERE tvid = %u;", tvid);
	return idExists(buff);
}

/**
 * Store the truthvalue of the atom.
 * Handle h must be the handle for the atom; its passed as an arg to
 * avoid having to look it up.
 */
int SQLAtomStorage::storeTruthValue(AtomPtr atom, Handle h)
{
	bool notfirst = false;
	std::string cols;
	std::string vals;
	std::string coda;

	const TruthValue &tv = atom->getTruthValue();

	const SimpleTruthValue *stv = dynamic_cast<const SimpleTruthValue *>(&tv);
	if (NULL == stv)
		throw IOException(TRACE_INFO, "Non-simple truth values are not handled\n");

	int tvid = TVID(tv);

	// If its a stock truth value, there is nothing to do.
	if (tvid <= 4) return tvid;

	// Use the TLB Handle as the UUID.
	char tvidbuff[BUFSZ];
	snprintf(tvidbuff, BUFSZ, "%u", tvid);

	bool update = tvExists(tvid);
	if (update)
	{
		cols = "UPDATE SimpleTVs SET ";
		vals = "";
		coda = " WHERE tvid = ";
		coda += tvidbuff;
		coda += ";";
	}
	else
	{
		cols = "INSERT INTO SimpleTVs (";
		vals = ") VALUES (";
		coda = ");";
		STMT("tvid", tvidbuff);
	}

	STMTF("mean", tv.getMean());
	STMTF("count", tv.getCount());

	std::string qry = cols + vals + coda;
	Response rp;
	rp.rs = db_conn->exec(qry.c_str());
	rp.release();

	return tvid;
}

/**
 * Return a new, unique ID for every truth value
 */
int SQLAtomStorage::TVID(const TruthValue &tv)
{
	if (tv == TruthValue::NULL_TV()) return 0;
	if (tv == TruthValue::TRIVIAL_TV()) return 1;
	if (tv == TruthValue::FALSE_TV()) return 2;
	if (tv == TruthValue::TRUE_TV()) return 3;
	if (tv == TruthValue::DEFAULT_TV()) return 4;

	Response rp;
	rp.rs = db_conn->exec("SELECT NEXTVAL('tvid_seq');");
	rp.rs->foreach_row(&Response::tvid_seq_cb, &rp);
	rp.release();
	return rp.tvid;
}

TruthValue* SQLAtomStorage::getTV(int tvid)
{
	if (0 == tvid) return (TruthValue *) & TruthValue::NULL_TV();
	if (1 == tvid) return (TruthValue *) & TruthValue::DEFAULT_TV();
	if (2 == tvid) return (TruthValue *) & TruthValue::FALSE_TV();
	if (3 == tvid) return (TruthValue *) & TruthValue::TRUE_TV();
	if (4 == tvid) return (TruthValue *) & TruthValue::TRIVIAL_TV();

	char buff[BUFSZ];
	snprintf(buff, BUFSZ, "SELECT * FROM SimpleTVs WHERE tvid = %u;", tvid);

	Response rp;
	rp.rs = db_conn->exec(buff);
	rp.rs->foreach_row(&Response::create_tv_cb, &rp);
	rp.release();

	SimpleTruthValue *stv = new SimpleTruthValue(rp.mean, rp.confidence);
	return stv;
}

#endif /* OUT_OF_LINE_TVS */

/* ================================================================== */

/**
 * Return largest distance from this atom to any node under it.
 * Nodes have a height of 0, by definition.  Links that contain only
 * nodes in their outgoing set have a height of 1, by definition.
 * The height of a link is, by definition, one more than the height
 * of the tallest atom in its outgoing set.
 * @note This can conversely be viewed as the depth of a tree.
 */
int SQLAtomStorage::get_height(const Handle& atom)
{
	if (not atom->isLink()) return 0;

	int maxd = 0;
	for (const Handle& h : atom->getOutgoingSet())
	{
		int d = get_height(h);
		if (maxd < d) maxd = d;
	}
	return maxd + 1;
}

/* ================================================================ */

UUID SQLAtomStorage::get_uuid(const Handle& h)
{
	UUID uuid = _tlbuf.getUUID(h);
	if (TLB::INVALID_UUID != uuid) return uuid;

	// Ooops. We need to find out what this is.
	TruthValuePtr tv;
	if (h->isNode())
	{
		tv = getNode(h->getType(), h->getName().c_str());
	}
	else
	{
		tv = getLink(h);
	}
	// If it was found, then the TLB got updated.
	if (tv) return _tlbuf.getUUID(h);

	// If it was not found, then issue a brand-spankin new UUID.
	return _tlbuf.addAtom(h, TLB::INVALID_UUID);
}

std::string SQLAtomStorage::oset_to_string(const HandleSeq& out)
{
	bool not_first = false;
	std::string str = "\'{";
	for (const Handle& h : out)
	{
		if (not_first) str += ", ";
		not_first = true;
		str += std::to_string(get_uuid(h));
	}
	str += "}\'";
	return str;
}

/* ================================================================ */

/// Drain the pending store queue.
///
/// Caution: this is ptentially racey in two different ways.
/// First, there is a small window in the async_caller implementation,
/// where, if the timing is just so, the barrier might return before
/// the last element is written.  Technically, that's a bug, but its
/// "minor" so we don't fix it.
///
/// The second issue is much more serious: We are NOT using any of the
/// transactional features in SQL, and so while we might have drained
/// the write queues here, on the client side, the SQL server will not
/// have actually commited the work by the time that this returns.
///
void SQLAtomStorage::flushStoreQueue()
{
	_write_queue.barrier();
}

/* ================================================================ */
/**
 * Recursively store the indicated atom, and all that it points to.
 * Store its truth values too. The recursive store is unconditional;
 * its assumed that all sorts of underlying truuth values have changed,
 * so that the whole thing needs to be stored.
 *
 * By default, the actual store is done asynchronously (in a different
 * thread); this routine merely queues up the atom. If the synchronous
 * flag is set, then the store is done in this thread.
 */
void SQLAtomStorage::storeAtom(const Handle& h, bool synchronous)
{
	get_ids();

	// If a synchronous store, avoid the queues entirely.
	if (synchronous)
	{
		do_store_atom(h);
		return;
	}
	_write_queue.enqueue(h);
}

/**
 * Synchronously store a single atom. That is, the actual store is done
 * in the calling thread.
 * Returns the height of the atom.
 */
int SQLAtomStorage::do_store_atom(const Handle& h)
{
	if (h->isNode())
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
	do_store_atom(h);
}

/* ================================================================ */
/**
 * Store just this one single atom (and its truth value).
 * Atoms in the outgoing set are NOT stored!
 * The store is performed synchronously (in the calling thread).
 */
void SQLAtomStorage::do_store_single_atom(const Handle& h, int aheight)
{
	setup_typemap();

	bool notfirst = false;
	std::string cols;
	std::string vals;
	std::string coda;

	// Use the TLB Handle as the UUID.
	UUID uuid = _tlbuf.addAtom(h, TLB::INVALID_UUID);

	std::string uuidbuff = std::to_string(uuid);

	std::unique_lock<std::mutex> lck = maybe_create_id(uuid);
	bool update = not lck.owns_lock();
	if (update)
	{
		cols = "UPDATE Atoms SET ";
		vals = "";
		coda = " WHERE uuid = ";
		coda += uuidbuff;
		coda += ";";
	}
	else
	{
		cols = "INSERT INTO Atoms (";
		vals = ") VALUES (";
		coda = ");";

		STMT("uuid", uuidbuff);
	}

#ifdef STORAGE_DEBUG
	if (0 == aheight) {
		if (update) _num_node_updates++; else _num_node_inserts++;
	} else {
		if (update) _num_link_updates++; else _num_link_inserts++;
	}
#endif // STORAGE_DEBUG

	// Store the atom type and node name only if storing for the
	// first time ever. Once an atom is in an atom table, it's type,
	// name or outset cannot be changed. Only its truth value can
	// change.
	if (false == update)
	{
		// Store the atomspace UUID
		AtomTable * at = getAtomTable(h);
		// We allow storage of atoms that don't belong to an atomspace.
		if (at) uuidbuff = std::to_string(at->get_uuid());
		else uuidbuff = "0";
		STMT("space", uuidbuff);

		// Store the atom UUID
		Type t = h->getType();
		int dbtype = storing_typemap[t];
		STMTI("type", dbtype);

		// Store the node name, if its a node
		if (h->isNode())
		{
			// Use postgres $-quoting to make unicode strings
			// easier to deal with.
			std::string qname = " $ocp$";
			qname += h->getName();
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

			if (h->isLink())
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
				if (330 < h->getArity())
				{
					throw IOException(TRACE_INFO,
						"Error: do_store_single_atom: Maxiumum Link size is 330.\n");
				}

				cols += ", outgoing";
				vals += ", ";
				vals += oset_to_string(h->getOutgoingSet());
			}
		}
	}

	// Store the truth value
	TruthValuePtr tv(h->getTruthValue());
	TruthValueType tvt = NULL_TRUTH_VALUE;
	if (tv) tvt = tv->getType();
	STMTI("tv_type", tvt);

	switch (tvt)
	{
		case NULL_TRUTH_VALUE:
			break;
		case SIMPLE_TRUTH_VALUE:
		case COUNT_TRUTH_VALUE:
		case PROBABILISTIC_TRUTH_VALUE:
			STMTF("stv_mean", tv->getMean());
			STMTF("stv_confidence", tv->getConfidence());
			STMTF("stv_count", tv->getCount());
			break;
		case INDEFINITE_TRUTH_VALUE:
		{
			IndefiniteTruthValuePtr itv = std::static_pointer_cast<const IndefiniteTruthValue>(tv);
			STMTF("stv_mean", itv->getL());
			STMTF("stv_count", itv->getU());
			STMTF("stv_confidence", itv->getConfidenceLevel());
			break;
		}
		default:
			throw IOException(TRACE_INFO,
				"Error: store_single: Unknown truth value type\n");
	}

	// We may have to store the atom table UUID and try again...
	// We waste CPU cycles to store the atomtable, only if it failed.
	bool try_again = false;
	std::string qry = cols + vals + coda;
	LLConnection* db_conn = get_conn();
	Response rp;
	rp.rs = db_conn->exec(qry.c_str());
	if (NULL == rp.rs) try_again = true;
	rp.release();

	if (try_again)
	{
		AtomTable *at = getAtomTable(h);
		if (at) store_atomtable_id(*at);
		rp.rs = db_conn->exec(qry.c_str());
		rp.release();
	}
	put_conn(db_conn);

	// Make note of the fact that this atom has been stored.
	add_id_to_cache(uuid);
	_store_count ++;
}

/* ================================================================ */
/**
 * Store the concordance of type names to type values.
 *
 * The concordance is used to match up the type id's stored in
 * the SQL database, against those currently in use in the current
 * version of the opencog server. The basic problem is that types
 * can be dynamic in OpenCog -- different versions will have
 * different types, and will assign different type numbers to some
 * given type name. To overcome this, the SQL database stores all
 * atoms according to the type *name* -- although, to save space, it
 * actually stored type ids; however, the SQL type-name-to-type-id
 * mapping can be completely different than the OpenCog type-name
 * to type-id mapping. Thus, tables to convert the one to the other
 * id are needed.
 *
 * Given an opencog type t, the storing_typemap[t] will contain the
 * sqlid for the named type. The storing_typemap[t] will *always*
 * contain a valid value.
 *
 * Given an SQL type sq, the loading_typemap[sq] will contain the
 * opencog type t for the named type, or NOTYPE if this version of
 * opencog does not have this kind of atom.
 *
 * The typemaps must be constructed before any saving or loading of
 * atoms can happen. The typemaps will be a superset (union) of the
 * types used by OpenCog, and stored in the SQL table.
 */
void SQLAtomStorage::setup_typemap(void)
{
	/* Only need to set up the typemap once. */
	if (type_map_was_loaded) return;

	/* Again, under the lock, so we don't race against ourselves. */
	std::lock_guard<std::mutex> lck(_typemap_mutex);
	if (type_map_was_loaded) return;

	// If we are here, we need to reconcile the types currently in
	// use, with a possibly pre-existing typemap. New types must be
	// stored.  So we start by loading a map from SQL (if its there).
	//
	// Be careful to initialize the typemap with invalid types,
	// in case there are unexpected holes in the map!
	for (int i=0; i< TYPEMAP_SZ; i++)
	{
		loading_typemap[i] = NOTYPE;
		storing_typemap[i] = -1;
		db_typename[i] = NULL;
	}

	LLConnection* db_conn = get_conn();
	Response rp;
	rp.rs = db_conn->exec("SELECT * FROM TypeCodes;");
	rp.store = this;
	rp.rs->foreach_row(&Response::type_cb, &rp);
	rp.release();

	unsigned int numberOfTypes = classserver().getNumberOfClasses();
	for (Type t=0; t<numberOfTypes; t++)
	{
		int sqid = storing_typemap[t];
		/* If this typename is not yet known, record it */
		if (-1 == sqid)
		{
			const char * tname = classserver().getTypeName(t).c_str();

			// Let the sql id be the same as the current type number,
			// unless this sql number is already in use, in which case
			// we need to find another, unused one.  Its in use if we
			// have a string name associated to it.
			sqid = t;

			if ((db_typename[sqid] != NULL) &&
				(loading_typemap[sqid] != t))
			{
				// Find some (any) unused type index to use in the
				// sql table. Use the lowest unused value that we
				// can find.
				for (sqid = 0; sqid<TYPEMAP_SZ; sqid++)
				{
					if (NULL == db_typename[sqid]) break;
				}

				if (TYPEMAP_SZ <= sqid)
				{
					put_conn(db_conn);
					OC_ASSERT("Fatal Error: type table overflow!\n");
				}
			}

			char buff[BUFSZ];
			snprintf(buff, BUFSZ,
			         "INSERT INTO TypeCodes (type, typename) "
			         "VALUES (%d, \'%s\');",
			         sqid, tname);
			rp.rs = db_conn->exec(buff);
			rp.release();
			set_typemap(sqid, tname);
		}
	}
	put_conn(db_conn);

	// Set this last!
	type_map_was_loaded = true;
}

void SQLAtomStorage::set_typemap(int dbval, const char * tname)
{
	Type realtype = classserver().getType(tname);
	loading_typemap[dbval] = realtype;
	storing_typemap[realtype] = dbval;
	if (db_typename[dbval] != NULL) free (db_typename[dbval]);
	db_typename[dbval] = strdup(tname);
}

/* ================================================================ */

/**
 * Add a single UUID to the ID cache. Thread-safe.
 * This also unlocks the id-creation lock, if it was being held.
 */
void SQLAtomStorage::add_id_to_cache(UUID uuid)
{
	std::unique_lock<std::mutex> lock(id_cache_mutex);
	local_id_cache.insert(uuid);

	// If we were previously making this ID, then we are done.
	// The other half of this is in maybe_create_id() below.
	if (0 < id_create_cache.count(uuid))
	{
		id_create_cache.erase(uuid);
	}
}

/**
 * This returns a lock that is either locked, or not, depending on
 * whether we think that the database already knows about this UUID,
 * or not.  We do this because we need to use an SQL INSERT instead
 * of an SQL UPDATE when putting a given atom in the database the first
 * time ever.  Since SQL INSERT can be used once and only once, we have
 * to avoid the case of two threads, each trying to perform an INSERT
 * in the same ID. We do this by taking the id_create_mutex, so that
 * only one writer ever gets told that its a new ID.
 *
 * This cannot be replaced by the new Postgres UPSERT command (well,
 * actually the INSERT ... ON CONFLICT UPDATE command) because we still
 * have to make sure that an atom is uniquely associated with a given
 * UUID, even if two different threads race, trying to store the same
 * atom. Otherwise, we risk inserting the same atom twice, with two
 * different UUID's. Whatever. The point is that the issuance of UUID's
 * is subtle, and can be bungled, if you're not careful.
 */
std::unique_lock<std::mutex> SQLAtomStorage::maybe_create_id(UUID uuid)
{
	std::unique_lock<std::mutex> create_lock(id_create_mutex);
	std::unique_lock<std::mutex> cache_lock(id_cache_mutex);
	// Look at the local cache of id's to see if the atom is in storage or not.
	if (0 < local_id_cache.count(uuid))
		return std::unique_lock<std::mutex>();

	// Is some other thread in the process of adding this ID?
	if (0 < id_create_cache.count(uuid))
	{
		cache_lock.unlock();
		while (true)
		{
			// If we are here, some other thread is making this UUID,
			// and so we need to wait till they're done. Wait by stalling
			// on the creation lock.
			std::unique_lock<std::mutex> local_create_lock(id_create_mutex);
			// If we are here, then someone finished creating some UUID.
			// Was it our ID? If so, we are done; if not, wait some more.
			cache_lock.lock();
			if (0 == id_create_cache.count(uuid))
			{
				OC_ASSERT(0 < local_id_cache.count(uuid),
					"Atom for UUID was not created!");
				return std::unique_lock<std::mutex>();
			}
			cache_lock.unlock();
		}
	}

	// If we are here, then no one has attempted to make this UUID before.
	// Grab the maker lock, and make the damned thing already.
	id_create_cache.insert(uuid);
	return create_lock;
}

/**
 * Build up a client-side cache of all atom id's in storage
 */
void SQLAtomStorage::get_ids(void)
{
	std::unique_lock<std::mutex> lock(id_cache_mutex);

	if (local_id_cache_is_inited) return;
	local_id_cache_is_inited = true;

	local_id_cache.clear();
	LLConnection* db_conn = get_conn();

	// It appears that, when the select statment returns more than
	// about a 100K to a million atoms or so, some sort of heap
	// corruption occurs in the odbc code, causing future mallocs
	// to fail. So limit the number of records processed in one go.
	// It also appears that asking for lots of records increases
	// the memory fragmentation (and/or there's a memory leak in odbc??)
#define USTEP 12003
	unsigned long rec;
	unsigned long max_nrec = getMaxObservedUUID();
	for (rec = 0; rec <= max_nrec; rec += USTEP)
	{
		char buff[BUFSZ];
		snprintf(buff, BUFSZ, "SELECT uuid FROM Atoms WHERE "
		         "uuid > %lu AND uuid <= %lu;",
		         rec, rec+USTEP);

		Response rp;
		rp.id_set = &local_id_cache;
		rp.rs = db_conn->exec(buff);
		rp.rs->foreach_row(&Response::note_id_cb, &rp);
		rp.release();
	}

	// Also get the ID's of the spaces that are in use.
	table_id_cache.clear();

	Response rp;
	rp.id_set = &table_id_cache;
	rp.rs = db_conn->exec("SELECT space FROM Spaces;");
	rp.rs->foreach_row(&Response::note_id_cb, &rp);
	rp.release();

	put_conn(db_conn);
}

/* ================================================================ */

/* One-size-fits-all atom fetcher */
SQLAtomStorage::PseudoPtr SQLAtomStorage::getAtom(const char * query, int height)
{
	LLConnection* db_conn = get_conn();
	Response rp;
	rp.uuid = TLB::INVALID_UUID;
	rp.rs = db_conn->exec(query);
	rp.rs->foreach_row(&Response::create_atom_cb, &rp);

	// Did we actually find anything?
	// DO NOT USE IsInvalidHandle() HERE! It won't work, duhh!
	if (rp.uuid == TLB::INVALID_UUID)
	{
		rp.release();
		put_conn(db_conn);
		return NULL;
	}

	rp.height = height;
	PseudoPtr atom(makeAtom(rp, rp.uuid));
	rp.release();
	put_conn(db_conn);
	return atom;
}

SQLAtomStorage::PseudoPtr SQLAtomStorage::petAtom(UUID uuid)
{
	setup_typemap();
	char buff[BUFSZ];
	snprintf(buff, BUFSZ, "SELECT * FROM Atoms WHERE uuid = %lu;", uuid);

	return getAtom(buff, -1);
}


/**
 * Retreive the entire incoming set of the indicated atom.
 */
HandleSeq SQLAtomStorage::getIncomingSet(const Handle& h)
{
	HandleSeq iset;

	setup_typemap();

	UUID uuid = _tlbuf.addAtom(h, TLB::INVALID_UUID);
	char buff[BUFSZ];
	snprintf(buff, BUFSZ,
		"SELECT * FROM Atoms WHERE outgoing @> ARRAY[CAST(%lu AS BIGINT)];",
		uuid);

	// Note: "select * from atoms where outgoing@>array[556];" will return
	// all links with atom 556 in the outgoing set -- i.e. the incoming set of 556.
	// Could also use && here instead of @> Don't know if one is faster or not.
	// The cast to BIGINT is needed, as otherwise on gets
	// ERROR:  operator does not exist: bigint[] @> integer[]

	LLConnection* db_conn = get_conn();
	Response rp;
	rp.store = this;
	rp.height = -1;
	rp.hvec = &iset;
	rp.rs = db_conn->exec(buff);
	rp.rs->foreach_row(&Response::fetch_incoming_set_cb, &rp);
	rp.release();
	put_conn(db_conn);

#ifdef STORAGE_DEBUG
	_num_get_insets++;
	_num_get_inatoms += iset.size();
#endif // STORAGE_DEBUG

	return iset;
}

/**
 * Fetch the TV, for the Node with the indicated type and name.
 * If there is no such node, NULL is returned.
 */
TruthValuePtr SQLAtomStorage::getNode(Type t, const char * str)
{
	setup_typemap();
	char buff[4*BUFSZ];

	// Use postgres $-quoting to make unicode strings easier to deal with.
	int nc = snprintf(buff, 4*BUFSZ, "SELECT * FROM Atoms WHERE "
		"type = %hu AND name = $ocp$%s$ocp$ ;", storing_typemap[t], str);

	if (4*BUFSZ-1 <= nc)
	{
		buff[4*BUFSZ-1] = 0x0;
		throw IOException(TRACE_INFO,
			"SQLAtomStorage::getNode: buffer overflow!\n"
			"\tnc=%d buffer=>>%s<<\n", nc, buff);
		return TruthValuePtr();
	}
#ifdef STORAGE_DEBUG
	_num_get_nodes++;
#endif // STORAGE_DEBUG

	PseudoPtr p(getAtom(buff, 0));
	if (NULL == p) return TruthValuePtr();

#ifdef STORAGE_DEBUG
	_num_got_nodes++;
#endif // STORAGE_DEBUG
	NodePtr node = createNode(t, str);
	_tlbuf.addAtom(node, p->uuid);
	return p->tv;
}

/**
 * Fetch TruthValue for the Link with given type and outgoing set.
 * If there is no such link, NULL is returned.
 */
TruthValuePtr SQLAtomStorage::getLink(const Handle& h)
{
	setup_typemap();

	char buff[BUFSZ];
	snprintf(buff, BUFSZ,
		"SELECT * FROM Atoms WHERE type = %hu AND outgoing = ",
		storing_typemap[h->getType()]);

	std::string ostr = buff;
	ostr += oset_to_string(h->getOutgoingSet());
	ostr += ";";

#ifdef STORAGE_DEBUG
	_num_get_links++;
#endif // STORAGE_DEBUG
	PseudoPtr p = getAtom(ostr.c_str(), 1);
	if (NULL == p) return TruthValuePtr();

#ifdef STORAGE_DEBUG
	_num_got_links++;
#endif // STORAGE_DEBUG
	_tlbuf.addAtom(h, p->uuid);
	return p->tv;
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
		((-1 == rp.height) and classserver().isA(realtype, NODE)))
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

	// Now get the truth value
	switch (rp.tv_type)
	{
		case NULL_TRUTH_VALUE:
			break;

		case SIMPLE_TRUTH_VALUE:
		{
			TruthValuePtr stv(SimpleTruthValue::createTV(rp.mean, rp.confidence));
			atom->tv = stv;
			break;
		}
		case COUNT_TRUTH_VALUE:
		{
			TruthValuePtr ctv(CountTruthValue::createTV(rp.mean, rp.confidence, rp.count));
			atom->tv = ctv;
			break;
		}
		case INDEFINITE_TRUTH_VALUE:
		{
			TruthValuePtr itv(IndefiniteTruthValue::createTV(rp.mean, rp.count, rp.confidence));
			atom->tv = itv;
			break;
		}
		case PROBABILISTIC_TRUTH_VALUE:
		{
			TruthValuePtr ptv(ProbabilisticTruthValue::createTV(rp.mean, rp.confidence, rp.count));
			atom->tv = ptv;
			break;
		}
		default:
			throw IOException(TRACE_INFO,
				"makeAtom: Unknown truth value type\n");
	}

	_load_count ++;
	if (bulk_load and _load_count%10000 == 0)
	{
		printf("\tLoaded %lu atoms.\n", (unsigned long) _load_count);
	}

	add_id_to_cache(uuid);
	return atom;
}

/* ================================================================ */

void SQLAtomStorage::load(AtomTable &table)
{
	unsigned long max_nrec = getMaxObservedUUID();
	_tlbuf.reserve_upto(max_nrec);
	printf("Max observed UUID is %lu\n", max_nrec);
	_load_count = 0;
	max_height = getMaxObservedHeight();
	printf("Max Height is %d\n", max_height);
	bulk_load = true;

	setup_typemap();

	LLConnection* db_conn = get_conn();
	Response rp;
	rp.table = &table;
	rp.store = this;

	for (int hei=0; hei<=max_height; hei++)
	{
		unsigned long cur = _load_count;

#if GET_ONE_BIG_BLOB
		char buff[BUFSZ];
		snprintf(buff, BUFSZ, "SELECT * FROM Atoms WHERE height = %d;", hei);
		rp.height = hei;
		rp.rs = db_conn->exec(buff);
		rp.rs->foreach_row(&Response::load_all_atoms_cb, &rp);
		rp.release();
#else
		// It appears that, when the select statement returns more than
		// about a 100K to a million atoms or so, some sort of heap
		// corruption occurs in the iodbc code, causing future mallocs
		// to fail. So limit the number of records processed in one go.
		// It also appears that asking for lots of records increases
		// the memory fragmentation (and/or there's a memory leak in iodbc??)
		// XXX Not clear is UnixSQL suffers from this same problem.
		// Whatever, seems to be a better strategy overall, anyway.
#define STEP 12003
		unsigned long rec;
		for (rec = 0; rec <= max_nrec; rec += STEP)
		{
			char buff[BUFSZ];
			snprintf(buff, BUFSZ, "SELECT * FROM Atoms WHERE "
			         "height = %d AND uuid > %lu AND uuid <= %lu;",
			         hei, rec, rec+STEP);
			rp.height = hei;
			rp.rs = db_conn->exec(buff);
			rp.rs->foreach_row(&Response::load_all_atoms_cb, &rp);
			rp.release();
		}
#endif
		printf("Loaded %lu atoms at height %d\n", _load_count - cur, hei);
	}
	put_conn(db_conn);
	printf("Finished loading %lu atoms in total\n",
		(unsigned long) _load_count);
	bulk_load = false;

	// synchrnonize!
	table.barrier();
}

void SQLAtomStorage::loadType(AtomTable &table, Type atom_type)
{
	unsigned long max_nrec = getMaxObservedUUID();
	_tlbuf.reserve_upto(max_nrec);
	logger().debug("SQLAtomStorage::loadType: Max observed UUID is %lu\n", max_nrec);
	_load_count = 0;

	// For links, assume a worst-case height.
	// For nodes, its easy ... max_height is zero.
	if (classserver().isNode(atom_type))
		max_height = 0;
	else
		max_height = getMaxObservedHeight();
	logger().debug("SQLAtomStorage::loadType: Max Height is %d\n", max_height);

	setup_typemap();
	int db_atom_type = storing_typemap[atom_type];

	LLConnection* db_conn = get_conn();
	Response rp;
	rp.table = &table;
	rp.store = this;

	for (int hei=0; hei<=max_height; hei++)
	{
		unsigned long cur = _load_count;

#if GET_ONE_BIG_BLOB
		char buff[BUFSZ];
		snprintf(buff, BUFSZ,
		         "SELECT * FROM Atoms WHERE height = %d AND type = %d;",
		         hei, db_atom_type);
		rp.height = hei;
		rp.rs = db_conn->exec(buff);
		rp.rs->foreach_row(&Response::load_if_not_exists_cb, &rp);
		rp.release();
#else
		// It appears that, when the select statment returns more than
		// about a 100K to a million atoms or so, some sort of heap
		// corruption occurs in the iodbc code, causing future mallocs
		// to fail. So limit the number of records processed in one go.
		// It also appears that asking for lots of records increases
		// the memory fragmentation (and/or there's a memory leak in iodbc??)
		// XXX Not clear is UnixSQL suffers from this same problem.
#define STEP 12003
		unsigned long rec;
		for (rec = 0; rec <= max_nrec; rec += STEP)
		{
			char buff[BUFSZ];
			snprintf(buff, BUFSZ, "SELECT * FROM Atoms WHERE type = %d "
			         "AND height = %d AND uuid > %lu AND uuid <= %lu;",
			         db_atom_type, hei, rec, rec+STEP);
			rp.height = hei;
			rp.rs = db_conn->exec(buff);
			rp.rs->foreach_row(&Response::load_if_not_exists_cb, &rp);
			rp.release();
		}
#endif
		logger().debug("SQLAtomStorage::loadType: Loaded %lu atoms of type %d at height %d\n",
			_load_count - cur, db_atom_type, hei);
	}
	put_conn(db_conn);
	logger().debug("SQLAtomStorage::loadType: Finished loading %lu atoms in total\n",
		(unsigned long) _load_count);

	// Synchronize!
	table.barrier();
}

void SQLAtomStorage::store_cb(const Handle& h)
{
	get_ids();
	int height = get_height(h);
	do_store_single_atom(h, height);

	if (_store_count%1000 == 0)
	{
		printf("\tStored %lu atoms.\n", (unsigned long) _store_count);
	}
}

void SQLAtomStorage::store(const AtomTable &table)
{
	max_height = 0;
	_store_count = 0;

#ifdef ALTER
	rename_tables();
	create_tables();
#endif

	get_ids();
	UUID max_uuid = _tlbuf.getMaxUUID();
	printf("Max UUID is %lu\n", max_uuid);

	setup_typemap();
	store_atomtable_id(table);

	LLConnection* db_conn = get_conn();
	Response rp;

	table.foreachHandleByType(
		[&](const Handle& h)->void { store_cb(h); }, ATOM, true);

	rp.rs = db_conn->exec("VACUUM ANALYZE;");
	rp.release();
	put_conn(db_conn);

	printf("\tFinished storing %lu atoms total.\n",
		(unsigned long) _store_count);
}

/* ================================================================ */

void SQLAtomStorage::rename_tables(void)
{
	LLConnection* db_conn = get_conn();
	Response rp;

	rp.rs = db_conn->exec("ALTER TABLE Atoms RENAME TO Atoms_Backup;");
	rp.release();
	rp.rs = db_conn->exec("ALTER TABLE Global RENAME TO Global_Backup;");
	rp.release();
	rp.rs = db_conn->exec("ALTER TABLE TypeCodes RENAME TO TypeCodes_Backup;");
	rp.release();
	put_conn(db_conn);
}

void SQLAtomStorage::create_tables(void)
{
	LLConnection* db_conn = get_conn();
	Response rp;

	// See the file "atom.sql" for detailed documentation as to the
	// structure of the SQL tables.
	rp.rs = db_conn->exec("CREATE TABLE Spaces ("
	                      "space     BIGINT PRIMARY KEY,"
	                      "parent    BIGINT);");
	rp.release();

	rp.rs = db_conn->exec("INSERT INTO Spaces VALUES (0,0);");
	rp.release();
	rp.rs = db_conn->exec("INSERT INTO Spaces VALUES (1,1);");
	rp.release();

	rp.rs = db_conn->exec("CREATE TABLE Atoms ("
	                      "uuid     BIGINT PRIMARY KEY,"
	                      "space    BIGINT REFERENCES spaces(space),"
	                      "type     SMALLINT,"
	                      "type_tv  SMALLINT,"
	                      "stv_mean FLOAT,"
	                      "stv_confidence FLOAT,"
	                      "stv_count DOUBLE PRECISION,"
	                      "height   SMALLINT,"
	                      "name     TEXT,"
	                      "outgoing BIGINT[],"
	                      "UNIQUE (type, name),"
	                      "UNIQUE (type, outgoing));");
	rp.release();

	rp.rs = db_conn->exec("CREATE TABLE TypeCodes ("
	                      "type SMALLINT UNIQUE,"
	                      "typename TEXT UNIQUE);");
	rp.release();
	type_map_was_loaded = false;

	put_conn(db_conn);
}

/**
 * kill_data -- destroy data in the database!! Dangerous !!
 * This routine is meant to be used only for running test cases.
 * It is extremely dangerous, as it can lead to total data loss.
 */
void SQLAtomStorage::kill_data(void)
{
	LLConnection* db_conn = get_conn();
	Response rp;

	// See the file "atom.sql" for detailed documentation as to the
	// structure of the SQL tables.
	rp.rs = db_conn->exec("DELETE from Atoms;");
	rp.release();

	// Delete the atomspaces as well!
	rp.rs = db_conn->exec("DELETE from Spaces;");
	rp.release();

	rp.rs = db_conn->exec("INSERT INTO Spaces VALUES (0,0);");
	rp.release();
	rp.rs = db_conn->exec("INSERT INTO Spaces VALUES (1,1);");
	rp.release();

	put_conn(db_conn);
}

/* ================================================================ */

UUID SQLAtomStorage::getMaxObservedUUID(void)
{
	LLConnection* db_conn = get_conn();
	Response rp;
	rp.intval = 0;
	rp.rs = db_conn->exec("SELECT uuid FROM Atoms ORDER BY uuid DESC LIMIT 1;");
	rp.rs->foreach_row(&Response::intval_cb, &rp);
	rp.release();
	put_conn(db_conn);
	return rp.intval;
}

int SQLAtomStorage::getMaxObservedHeight(void)
{
	LLConnection* db_conn = get_conn();
	Response rp;
	rp.intval = 0;
	rp.rs = db_conn->exec("SELECT height FROM Atoms ORDER BY height DESC LIMIT 1;");
	rp.rs->foreach_row(&Response::intval_cb, &rp);
	rp.release();
	put_conn(db_conn);
	return rp.intval;
}

void SQLAtomStorage::reserve(void)
{
	UUID max_observed_id = getMaxObservedUUID();
	printf("Reserving UUID up to %lu\n", max_observed_id);
	_tlbuf.reserve_upto(max_observed_id);
}

/* ================================================================ */

void SQLAtomStorage::print_stats(void)
{
	printf("\n");
	size_t load_count = _load_count;
	size_t store_count = _store_count;
	double frac = store_count / ((double) load_count);
	printf("sql-stats: total loads = %lu total stores = %lu ratio=%f\n",
	       load_count, store_count, frac);
	printf("\n");

#ifdef STORAGE_DEBUG
	size_t num_get_nodes = _num_get_nodes;
	size_t num_got_nodes = _num_got_nodes;
	size_t num_get_links = _num_get_links;
	size_t num_got_links = _num_got_links;
	size_t num_get_insets = _num_get_insets;
	size_t num_get_inatoms = _num_get_inatoms;
	size_t num_node_inserts = _num_node_inserts;
	size_t num_node_updates = _num_node_updates;
	size_t num_link_inserts = _num_link_inserts;
	size_t num_link_updates = _num_link_updates;

	frac = 100.0 * num_got_nodes / ((double) num_get_nodes);
	printf("num_get_nodes=%lu num_got_nodes=%lu (%f pct)\n",
	       num_get_nodes, num_got_nodes, frac);

	frac = 100.0 * num_got_links / ((double) num_get_links);
	printf("num_get_links=%lu num_got_links=%lu (%f pct)\n",
	       num_get_links, num_got_links, frac);

	frac = num_get_inatoms / ((double) num_get_insets);
	printf("num_get_insets=%lu num_get_inatoms=%lu ratio=%f\n",
	       num_get_insets, num_get_inatoms, frac);

	frac = num_node_updates / ((double) num_node_inserts);
	printf("num_node_inserts=%lu num_node_updates=%lu ratio=%f\n",
	       num_node_inserts, num_node_updates, frac);

	frac = num_link_updates / ((double) num_link_inserts);
	printf("num_link_inserts=%lu num_link_updates=%lu ratio=%f\n",
	       num_link_inserts, num_link_updates, frac);

	unsigned long tot_node = num_node_inserts + num_node_updates;
	unsigned long tot_link = num_link_inserts + num_link_updates;
	frac = tot_node / ((double) tot_link);
	printf("total stores for node=%lu link=%lu ratio=%f\n",
	       tot_node, tot_link, frac);
#endif // STORAGE_DEBUG

	// Store queue performance
	unsigned long item_count = _write_queue._item_count;
	unsigned long flush_count = _write_queue._flush_count;
	unsigned long drain_count = _write_queue._drain_count;
	unsigned long drain_msec = _write_queue._drain_msec;
	unsigned long drain_slowest_msec = _write_queue._drain_slowest_msec;
	unsigned long drain_concurrent = _write_queue._drain_concurrent;

	double flush_frac = item_count / ((double) flush_count);
	double fill_frac = item_count / ((double) drain_count);

	unsigned long dentries = drain_count + drain_concurrent;
	double drain_ratio = dentries / ((double) drain_count);
	double drain_secs = 0.001 * drain_msec / ((double) dentries);
	double slowest = 0.001 * drain_slowest_msec;

	printf("\n");
	printf("write items=%lu flushes=%lu flush_ratio=%f\n",
	       item_count, flush_count, flush_frac);
	printf("drains=%lu fill_fraction=%f concurrency=%f\n",
	       drain_count, fill_frac, drain_ratio);
	printf("avg drain time=%f seconds; longest drain time=%f\n",
	       drain_secs, slowest);

	printf("currently in_drain=%d num_busy=%lu queue_size=%lu\n",
	       _write_queue._in_drain, _write_queue.get_busy_writers(),
	       _write_queue.get_queue_size());

	printf("current conn_pool free=%u of %d\n", conn_pool.size(),
	       _initial_conn_pool_size);

	// Some basic TLB statistics; could be improved;
	// The TLB remapping theory needs some work...
	size_t noh = 0;
	// size_t remap = 0;

	UUID mad = _tlbuf.getMaxUUID();
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

	frac = 100.0 * noh / ((double) mad);
	printf("sql-stats: %lu of %lu uuids unused (%f pct)\n",
	       noh, mad, frac);
}

#endif /* HAVE_SQL_STORAGE */
/* ============================= END OF FILE ================= */
