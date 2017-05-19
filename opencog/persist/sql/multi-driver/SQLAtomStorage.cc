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
#ifdef HAVE_SQL_STORAGE

#include <stdlib.h>
#include <times.h>
#include <unistd.h>

#include <chrono>
#include <memory>
#include <thread>

#include <opencog/util/oc_assert.h>
#include <opencog/util/oc_omp.h>

#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/base/ClassServer.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/base/FloatValue.h>
#include <opencog/atoms/base/LinkValue.h>
#include <opencog/atoms/base/StringValue.h>
#include <opencog/atoms/base/Valuation.h>
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
		Type itype;
		const char * name;
		const char *outlist;
		int height;

		// Values
		double *floatval;
		const char *stringval;
		UUID *linkval;

	private:
		concurrent_stack<LLConnection*>& _pool;
		LLConnection* _conn;

	public:
		Response(concurrent_stack<LLConnection*>& pool) : _pool(pool)
		{
			tname = "";
			itype = 0;
			intval = 0;
			_conn = nullptr;
			rs = nullptr;
		}

		~Response()
		{
			if (rs) rs->release();
			rs = nullptr;

			// Put the SQL connection back into the pool.
			if (_conn) _pool.push(_conn);
			_conn = nullptr;
		}

		void exec(const char * buff)
		{
			if (rs) rs->release();

			// Get an SQL connection.  If the pool is empty, this will
			// block, waiting for a connection to be returned to the pool.
			// Thus, the size of the pool regulates how many outstanding
			// SQL requests can be pending in parallel.
			if (nullptr == _conn) _conn = _pool.pop();
			rs = _conn->exec(buff);
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

			return true;
		}

		AtomTable *table;
		SQLAtomStorage *store;
		bool load_all_atoms_cb(void)
		{
			// printf ("---- New atom found ----\n");
			rs->foreach_column(&Response::create_atom_column_cb, this);

			PseudoPtr p(store->makeAtom(*this, uuid));

			// Corrupted databases can cause get_recursive_if_not_exists
			// to throw an exception. Skip the offending atom, and carry
			// on.
			try
			{
				Handle atom(store->get_recursive_if_not_exists(p));
				Handle h(table->add(atom, false));

				// Force resolution in TLB, so that later removes work.
				store->_tlbuf.addAtom(h, uuid);

				// Get the values only after TLB insertion!!
				store->get_atom_values(h);
			}
			catch (const IOException& ex) {}

			return false;
		}

		// Load an atom into the atom table. Fetch all values on the
		// atom, but NOT on its outgoing set!
		bool load_if_not_exists_cb(void)
		{
			// printf ("---- New atom found ----\n");
			rs->foreach_column(&Response::create_atom_column_cb, this);

			Handle h(store->_tlbuf.getAtom(uuid));
			if (nullptr == h)
			{
				PseudoPtr p(store->makeAtom(*this, uuid));
				Handle atom(store->get_recursive_if_not_exists(p));
				h = table->getHandle(atom);
				if (nullptr == h)
				{
					h = table->add(atom, false);
					store->_tlbuf.addAtom(h, uuid);
				}
			}

			// Clobber all values, including truth values.
			store->get_atom_values(h);
			return false;
		}

		std::vector<PseudoPtr> *pvec;
		bool fetch_incoming_set_cb(void)
		{
			// printf ("---- New atom found ----\n");
			rs->foreach_column(&Response::create_atom_column_cb, this);

			// Note, unlike the above 'load' routines, this merely fetches
			// the atoms, and returns a vector of them.  They are loaded
			// into the atomspace later, by the caller.
			pvec->emplace_back(store->makeAtom(*this, uuid));
			return false;
		}

		// deal with the type-to-id map
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

		// Values ---------------------------------------------------
		// Callbacks for Values and Valuations.
		// The table layout for values and valuations are almost
		// identical, so we use common code for both.
		VUID vuid;
		Type vtype;
		const char * fltval;
		const char * strval;
		const char * lnkval;
		UUID key;
		bool get_value_cb(void)
		{
			rs->foreach_column(&Response::get_value_column_cb, this);
			// Returning true halts the callback after one row.  The
			// ODBC driver will clobber empty rows, so this is needed.
			return true;
		}
		bool get_value_column_cb(const char *colname, const char * colvalue)
		{
			// printf ("value -- %s = %s\n", colname, colvalue);
			if (!strcmp(colname, "floatvalue"))
			{
				fltval = colvalue;
			}
			else if (!strcmp(colname, "stringvalue"))
			{
				strval = colvalue;
			}
			else if (!strcmp(colname, "linkvalue"))
			{
				lnkval = colvalue;
			}
			else if (!strcmp(colname, "type"))
			{
				vtype = atoi(colvalue);
			}
			else if (!strcmp(colname, "key"))
			{
				key = atol(colvalue);
			}
			return false;
		}
		Handle atom;
		bool get_all_values_cb(void)
		{
			rs->foreach_column(&Response::get_value_column_cb, this);

			Handle hkey(store->_tlbuf.getAtom(key));
			if (nullptr == hkey)
			{
				PseudoPtr pu(store->petAtom(key));
				hkey = store->get_recursive_if_not_exists(pu);
			}

			ProtoAtomPtr pap = store->doUnpackValue(*this);
			atom->setValue(hkey, pap);

			// Special case for truth values
			if (classserver().isA(pap->getType(), TRUTH_VALUE))
			{
				TruthValuePtr tv(std::dynamic_pointer_cast<TruthValue>(pap));
				atom->setTruthValue(tv);
			}
			return false;
		}

		// Get generic positive integer values
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
};

/* ================================================================ */
// Constructors

// Initialize with 8 write-back queues, but maybe this should be...
// I dunno -- std::thread::hardware_concurrency()  ???
#define NUM_WB_QUEUES 8

#define STORAGE_DEBUG 1

void SQLAtomStorage::init(const char * uri)
{
	bool use_libpq = (0 == strncmp(uri, "postgres", 8));
	bool use_odbc = (0 == strncmp(uri, "odbc", 4));

	// default to postgres, if no driver given.
	if (uri[0] == '/') use_libpq = true;

	if (not use_libpq and not use_odbc)
		throw IOException(TRACE_INFO, "Unknown URI '%s'\n", uri);

	// Allow for one connection per database-reader, and one connection
	// for each writer.  Make sure that there are more connections than
	// there are writers, else both readers and writers starve.
	_initial_conn_pool_size = std::thread::hardware_concurrency();
	if (0 == _initial_conn_pool_size) _initial_conn_pool_size = 8;
	_initial_conn_pool_size += NUM_WB_QUEUES;
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

	Response rp(conn_pool);
	rp.exec(buff);
}


/* ================================================================ */

#define STMT(colname,val) { \
	if (notfirst) { cols += ", "; vals += ", "; } else notfirst = true; \
	cols += colname; \
	vals += val; \
}

#define STMTI(colname,ival) { \
	char buff[BUFSZ]; \
	snprintf(buff, BUFSZ, "%u", ival); \
	STMT(colname, buff); \
}

#define STMTF(colname,fval) { \
	char buff[BUFSZ]; \
	snprintf(buff, BUFSZ, "%22.16g", fval); \
	STMT(colname, buff); \
}

/* ================================================================ */

/// Delete the valuation, if it exists. This is required, in order
/// to prevent garbage from accumulating in the Values table.
/// It also simplifies, ever-so-slightly, the update of valuations.
void SQLAtomStorage::deleteValuation(const Handle& key, const Handle& atom)
{
	char buff[BUFSZ];
	snprintf(buff, BUFSZ,
		"SELECT * FROM Valuations WHERE key = %lu AND atom = %lu;",
		get_uuid(key), get_uuid(atom));

	Response rp(conn_pool);
	rp.vtype = 0;
	rp.exec(buff);
	rp.rs->foreach_row(&Response::get_value_cb, &rp);

	if (LINK_VALUE == rp.vtype)
	{
		const char *p = rp.lnkval;
		if (p and *p == '{') p++;
		while (p)
		{
			if (*p == '}' or *p == '\0') break;
			VUID vu = atol(p);
			deleteValue(vu);
			p = strchr(p, ',');
			if (p) p++;
		}
	}

	if (0 != rp.vtype)
	{
		snprintf(buff, BUFSZ,
			"DELETE FROM Valuations WHERE key = %lu AND atom = %lu;",
			get_uuid(key), get_uuid(atom));

		rp.exec(buff);
	}
}

/**
 * Store a valuation. Return an integer ID for that valuation.
 * Thread-safe.
 */
void SQLAtomStorage::storeValuation(const ValuationPtr& valn)
{
	storeValuation(valn->key(), valn->atom(), valn->value());
}

void SQLAtomStorage::storeValuation(const Handle& key,
                                    const Handle& atom,
                                    const ProtoAtomPtr& pap)
{
	bool notfirst = false;
	std::string cols;
	std::string vals;
	std::string coda;

	// Get UUID from the TLB.
	UUID kuid = check_uuid(key);
	if (TLB::INVALID_UUID == kuid)
	{
		do_store_atom(key);
		kuid = get_uuid(key);
	}
	char kidbuff[BUFSZ];
	snprintf(kidbuff, BUFSZ, "%lu", kuid);

	char aidbuff[BUFSZ];
	UUID auid = get_uuid(atom);
	snprintf(aidbuff, BUFSZ, "%lu", auid);

	// The prior valuation, if any, will be deleted firest,
	// and so an INSERT is sufficient to cover everything.
	cols = "INSERT INTO Valuations (";
	vals = ") VALUES (";
	coda = ");";
	STMT("key", kidbuff);
	STMT("atom", aidbuff);

	Type vtype = pap->getType();
	STMTI("type", storing_typemap[vtype]);

	if (classserver().isA(vtype, FLOAT_VALUE))
	{
		FloatValuePtr fvp = FloatValueCast(pap);
		std::string fstr = float_to_string(fvp);
		STMT("floatvalue", fstr);
	}
	else
	if (classserver().isA(vtype, STRING_VALUE))
	{
		StringValuePtr fvp = StringValueCast(pap);
		std::string sstr = string_to_string(fvp);
		STMT("stringvalue", sstr);
	}
	else
	if (classserver().isA(vtype, LINK_VALUE))
	{
		LinkValuePtr fvp = LinkValueCast(pap);
		std::string lstr = link_to_string(fvp);
		STMT("linkvalue", lstr);
	}

	std::string insert = cols + vals + coda;

	std::lock_guard<std::mutex> lck(_value_mutex[auid%NUMVMUT]);
	// Use a transaction, so that other threads/users see the
	// valuation update atomically. That is, two sets of
	// users/threads can safely set the same valuation at the same
	// time. A third thread will always see an appropriate valuation,
	// either the earlier one, or the newer one.
	// XXX At least, that was the hope. In practice, this is not working
	// as designed. fixme later.
	Response rp(conn_pool);
	rp.exec("BEGIN");

	// If there's an existing valuation, delete it.
	deleteValuation(key, atom);

	rp.exec(insert.c_str());
	rp.exec("COMMIT");

	_valuation_stores++;
}

// Almost a cut-n-passte of the above, but different.
SQLAtomStorage::VUID SQLAtomStorage::storeValue(const ProtoAtomPtr& pap)
{
	VUID vuid = _next_valid++;

	bool notfirst = false;
	std::string cols;
	std::string vals;
	std::string coda;

	cols = "INSERT INTO Values (";
	vals = ") VALUES (";
	coda = ");";
	STMT("vuid", std::to_string(vuid));

	Type vtype = pap->getType();
	STMTI("type", storing_typemap[vtype]);

	if (classserver().isA(vtype, FLOAT_VALUE))
	{
		FloatValuePtr fvp = FloatValueCast(pap);
		std::string fstr = float_to_string(fvp);
		STMT("floatvalue", fstr);
	}
	else
	if (classserver().isA(vtype, STRING_VALUE))
	{
		StringValuePtr fvp = StringValueCast(pap);
		std::string sstr = string_to_string(fvp);
		STMT("stringvalue", sstr);
	}
	else
	if (classserver().isA(vtype, LINK_VALUE))
	{
		LinkValuePtr fvp = LinkValueCast(pap);
		std::string lstr = link_to_string(fvp);
		STMT("linkvalue", lstr);
	}

	std::string qry = cols + vals + coda;
	Response rp(conn_pool);
	rp.exec(qry.c_str());

	_value_stores++;
	return vuid;
}

/// Return a value, given by the VUID identifier, taken from the
/// Values table. If the value type is a link, then the full recursive
/// fetch is performed.
ProtoAtomPtr SQLAtomStorage::getValue(VUID vuid)
{
	char buff[BUFSZ];
	snprintf(buff, BUFSZ, "SELECT * FROM Values WHERE vuid = %lu;", vuid);
	return doGetValue(buff);
}

/// Return a value, given by the key-atom pair.
/// If the value type is a link, then the full recursive
/// fetch is performed.
ProtoAtomPtr SQLAtomStorage::getValuation(const Handle& key,
                                          const Handle& atom)
{
	char buff[BUFSZ];
	snprintf(buff, BUFSZ,
		"SELECT * FROM Valuations WHERE key = %lu AND atom = %lu;",
		get_uuid(key),
		get_uuid(atom));

	return doGetValue(buff);
}

/// Return a value, given by indicated query buffer.
/// If the value type is a link, then the full recursive
/// fetch is performed.
ProtoAtomPtr SQLAtomStorage::doGetValue(const char * buff)
{
	Response rp(conn_pool);
	rp.exec(buff);
	rp.rs->foreach_row(&Response::get_value_cb, &rp);
   return doUnpackValue(rp);
}

/// Return a value, given by indicated query buffer.
/// If the value type is a link, then the full recursive
/// fetch is performed.
ProtoAtomPtr SQLAtomStorage::doUnpackValue(Response& rp)
{
	// Convert from databasse type to C++ runtime type
	Type vtype = loading_typemap[rp.vtype];

	// We expect rp.strval to be of the form
	// {aaa,"bb bb bb","ccc ccc ccc"}
	// Split it along the commas.
	if (vtype == STRING_VALUE)
	{
		std::vector<std::string> strarr;
		char *s = strdup(rp.strval);
		char *p = s;
		if (p and *p == '{') p++;
		while (p)
		{
			if (*p == '}' or *p == '\0') break;
			// String terminates at comma or close-brace.
			char * c = strchr(p, ',');
			if (c) *c = 0;
			else c = strchr(p, '}');
			if (c) *c = 0;

			// Wipe out quote marks
			if (*p == '"') p++;
			if (c and *(c-1) == '"') *(c-1) = 0;

			strarr.emplace_back(p);
			p = c;
			p++;
		}
		free(s);
		return createStringValue(strarr);
	}

	// We expect rp.fltval to be of the form
	// {1.1,2.2,3.3}
	if ((vtype == FLOAT_VALUE)
	    or classserver().isA(vtype, TRUTH_VALUE))
	{
		std::vector<double> fltarr;
		char *p = (char *) rp.fltval;
		if (p and *p == '{') p++;
		while (p)
		{
			if (*p == '}' or *p == '\0') break;
			double flt = strtod(p, &p);
			fltarr.emplace_back(flt);
			p++; // skip over  comma
		}
		if (vtype == FLOAT_VALUE)
			return createFloatValue(fltarr);
		else
			return ProtoAtomCast(TruthValue::factory(vtype, fltarr));
	}

	// We expect rp.lnkval to be a comma-separated list of
	// vuid's, which we then fetch recursively.
	if (vtype == LINK_VALUE)
	{
		std::vector<ProtoAtomPtr> lnkarr;
		const char *p = rp.lnkval;
		if (p and *p == '{') p++;
		while (p)
		{
			if (*p == '}' or *p == '\0') break;
			VUID vu = atol(p);
			ProtoAtomPtr pap = getValue(vu);
			lnkarr.emplace_back(pap);
			p = strchr(p, ',');
			if (p) p++;
		}
		return createLinkValue(lnkarr);
	}

	throw IOException(TRACE_INFO, "Unexpected value type=%d", rp.vtype);
	return nullptr;
}

void SQLAtomStorage::deleteValue(VUID vuid)
{
	char buff[BUFSZ];
	snprintf(buff, BUFSZ, "SELECT * FROM Values WHERE vuid = %lu;", vuid);

	Response rp(conn_pool);
	rp.exec(buff);
	rp.rs->foreach_row(&Response::get_value_cb, &rp);

	// Perform a recursive delete, if necessary.
	// We expect rp.strval to be of the form
	// {81,82,83} -- Split it along the commas.
	if (rp.vtype == LINK_VALUE)
	{
		const char *p = rp.lnkval;
		if (p and *p == '{') p++;
		while (p)
		{
			if (*p == '}' or *p == '\0') break;
			VUID vu = atol(p);
			deleteValue(vu);
			p = strchr(p, ',');
			if (p) p++;
		}
	}

	snprintf(buff, BUFSZ, "DELETE FROM Values WHERE vuid = %lu;", vuid);
	rp.exec(buff);
}

/// Store ALL of the values associated with the atom.
void SQLAtomStorage::store_atom_values(const Handle& atom)
{
	std::set<Handle> keys = atom->getKeys();
	for (const Handle& key: keys)
	{
		ProtoAtomPtr pap = atom->getValue(key);
		storeValuation(key, atom, pap);
	}

	// Special-case for TruthValues. Can we get rid of this someday?
	TruthValuePtr tv(atom->getTruthValue());

	// XXX This is if-check costs cycles; do we really
	// need to do this?
	if (tv->isDefaultTV())
	{
		deleteValuation(tvpred, atom);
		return;
	}

	storeValuation(tvpred, atom, ProtoAtomCast(tv));
}

/// Get ALL of the values associated with an atom.
void SQLAtomStorage::get_atom_values(Handle& atom)
{
	if (nullptr == atom) return;

	char buff[BUFSZ];
	snprintf(buff, BUFSZ,
		"SELECT * FROM Valuations WHERE atom = %lu;",
		get_uuid(atom));

	Response rp(conn_pool);
	rp.exec(buff);

	rp.store = this;
	rp.atom = atom;
	rp.rs->foreach_row(&Response::get_all_values_cb, &rp);
	rp.atom = nullptr;
}

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

/// Return the UUID of the handle, if it is known.
/// If the handle is in the database, then the correct UUID is returned.
/// If the handle is NOT in the database, this returns the invalid UUID.
UUID SQLAtomStorage::check_uuid(const Handle& h)
{
	UUID uuid = _tlbuf.getUUID(h);
	if (TLB::INVALID_UUID != uuid) return uuid;

	// Ooops. We need to find out what this is.
	Handle dbh;
	if (h->isNode())
	{
		dbh = doGetNode(h->getType(), h->getName().c_str());
	}
	else
	{
		dbh = doGetLink(h->getType(), h->getOutgoingSet());
	}
	// If it was found, then the TLB got updated.
	if (dbh) return _tlbuf.getUUID(h);

	// If it was not found, then say so.
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

std::string SQLAtomStorage::float_to_string(const FloatValuePtr& fvle)
{
	bool not_first = false;
	std::string str = "\'{";
	for (double v : fvle->value())
	{
		if (not_first) str += ", ";
		not_first = true;
		str += std::to_string(v);
	}
	str += "}\'";
	return str;
}

std::string SQLAtomStorage::string_to_string(const StringValuePtr& svle)
{
	bool not_first = false;
	std::string str = "\'{";
	for (const std::string& v : svle->value())
	{
		if (not_first) str += ", ";
		not_first = true;
		str += v;
	}
	str += "}\'";
	return str;
}

std::string SQLAtomStorage::link_to_string(const LinkValuePtr& lvle)
{
	bool not_first = false;
	std::string str = "\'{";
	for (const ProtoAtomPtr& pap : lvle->value())
	{
		if (not_first) str += ", ";
		not_first = true;
		VUID vuid = storeValue(pap);
		str += std::to_string(vuid);
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
		do_store_atom(h);
		return;
	}
	_write_queue.enqueue(h);
}

/**
 * Synchronously store a single atom. That is, the actual store is done
 * in the calling thread.  All values attached to the atom are also
 * stored. All values attached to atoms in the outgoing set are also
 * stored.
 *
 * Returns the height of the atom.
 */
int SQLAtomStorage::do_store_atom(const Handle& h)
{
	if (h->isNode())
	{
		do_store_single_atom(h, 0);
		store_atom_values(h);
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
	store_atom_values(h);
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

	std::lock_guard<std::mutex> create_lock(_store_mutex);

	// Lets see if we already know about this
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

	{
		Response rp(conn_pool);
		rp.exec("SELECT * FROM TypeCodes;");
		rp.store = this;
		rp.rs->foreach_row(&Response::type_cb, &rp);
	}

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
					OC_ASSERT("Fatal Error: type table overflow!\n");
				}
			}
			set_typemap(sqid, tname);

			char buff[BUFSZ];
			snprintf(buff, BUFSZ,
			         "INSERT INTO TypeCodes (type, typename) "
			         "VALUES (%d, \'%s\');",
			         sqid, tname);
			Response rp(conn_pool);
			rp.exec(buff);
		}
	}

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
	if (classserver().isA(p->type, NODE))
	{
		NodePtr node(createNode(p->type, p->name));
		_tlbuf.addAtom(node, p->uuid);
		_num_rec_nodes ++;
		return node->getHandle();
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
	LinkPtr link(createLink(resolved_oset, p->type));
	_tlbuf.addAtom(link, p->uuid);
	_num_rec_links++;
	return link->getHandle();
}

/**
 * Retreive the incoming set of the indicated atom.
 */
void SQLAtomStorage::getIncoming(AtomTable& table, const char *buff)
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

	// A parallel fetch is much much faster, esp for big osets.
	// std::for_each(std::execution::par_unseq, ... requires C++17
	OMP_ALGO::for_each(pset.begin(), pset.end(),
		[&] (const PseudoPtr& p)
	{
		Handle hi(get_recursive_if_not_exists(p));
		hi = table.add(hi, false);
		_tlbuf.addAtom(hi, p->uuid);
		get_atom_values(hi);
		std::lock_guard<std::mutex> lck(iset_mutex);
		iset.emplace_back(hi);
	});

#ifdef STORAGE_DEBUG
	_num_get_insets++;
	_num_get_inlinks += iset.size();
#endif // STORAGE_DEBUG
}

/**
 * Retreive the entire incoming set of the indicated atom.
 */
void SQLAtomStorage::getIncomingSet(AtomTable& table, const Handle& h)
{
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
void SQLAtomStorage::getIncomingByType(AtomTable& table, const Handle& h, Type t)
{
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
		"type = %hu AND name = $ocp$%s$ocp$ ;", storing_typemap[t], str);

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
		"SELECT * FROM Atoms WHERE type = %hu AND outgoing = ",
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

	_load_count ++;
	if (bulk_load and _load_count%10000 == 0)
	{
		time_t secs = time(0) - bulk_start;
		double rate = ((double) _load_count) / secs;
		printf("\tLoaded %lu atoms (%f per second).\n",
			(unsigned long) _load_count, rate);
	}

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
	bulk_start = time(0);

	setup_typemap();

#define NCHUNKS 300
#define MINSTEP 10123
	std::vector<unsigned long> steps;
	unsigned long stepsize = MINSTEP + max_nrec/NCHUNKS;
	for (unsigned long rec = 0; rec <= max_nrec; rec += stepsize)
		steps.push_back(rec);

	printf("Loading all atoms: "
		"Max Height is %d stepsize=%lu chunks=%lu\n",
		 max_height, stepsize, steps.size());

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
	logger().debug("SQLAtomStorage::loadType: Finished loading %lu atoms in total\n",
		(unsigned long) _load_count);

	// Synchronize!
	table.barrier();
}

void SQLAtomStorage::store(const AtomTable &table)
{
	max_height = 0;
	_store_count = 0;

#ifdef ALTER
	rename_tables();
	create_tables();
#endif

	UUID max_uuid = _tlbuf.getMaxUUID();
	printf("Max UUID is %lu\n", max_uuid);

	setup_typemap();
	store_atomtable_id(table);

	// table.foreachHandleByType(
	table.foreachParallelByType(
		[&](const Handle& h)->void
	{
		do_store_atom(h);
		if (_store_count%1000 == 0)
			printf("\tStored %lu atoms.\n", (unsigned long) _store_count);

	}, ATOM, true);

	Response rp(conn_pool);
	rp.exec("VACUUM ANALYZE Atoms;");

	printf("\tFinished storing %lu atoms total.\n",
		(unsigned long) _store_count);
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

void SQLAtomStorage::reserve(void)
{
	UUID max_observed_id = getMaxObservedUUID();
	printf("Reserving UUID up to %lu\n", max_observed_id);
	_tlbuf.reserve_upto(max_observed_id);
}

void SQLAtomStorage::clear_cache(void)
{
	_tlbuf.clear();
	reserve();
}

/* ================================================================ */

void SQLAtomStorage::clear_stats(void)
{
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
#endif // STORAGE_DEBUG
}

void SQLAtomStorage::print_stats(void)
{
	size_t load_count = _load_count;
	size_t store_count = _store_count;
	double frac = store_count / ((double) load_count);
	printf("sql-stats: total loads = %lu total stores = %lu ratio=%f\n",
	       load_count, store_count, frac);

	size_t valuation_stores = _valuation_stores;
	size_t value_stores = _value_stores;
	printf("sql-stats: valuation updates = %lu value updates = %lu\n",
	       valuation_stores, value_stores);
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
	printf("num_get_nodes=%lu num_got_nodes=%lu (%f pct)\n",
	       num_get_nodes, num_got_nodes, frac);
	printf("num_recursive_nodes=%lu\n", num_rec_nodes);

	frac = 100.0 * num_got_links / ((double) num_get_links);
	printf("num_get_links=%lu num_got_links=%lu (%f pct)\n",
	       num_get_links, num_got_links, frac);
	printf("num_recursive_links=%lu\n", num_rec_links);

	frac = num_get_inlinks / ((double) num_get_insets);
	printf("num_get_incoming_sets=%lu set total=%lu avg set size=%f\n",
	       num_get_insets, num_get_inlinks, frac);

	unsigned long tot_node = num_node_inserts;
	unsigned long tot_link = num_link_inserts;
	frac = tot_link / ((double) tot_node);
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

	size_t used = _tlbuf.size();
	frac = 100.0 * used / ((double) mad);
	printf("sql-stats: %lu of %lu reserved uuids used (%f pct)\n",
	       used, mad, frac);
}

#endif /* HAVE_SQL_STORAGE */
/* ============================= END OF FILE ================= */
