/*
 * SQLResponse.h
 * Utility for parsing results returned from SQL.
 *
 * The SQL queries return rows and columns; these need to be
 * re-assembled into Atoms and Values. This is a utility class that
 * aids in this re-assembly. This interfaces to one of the "LLAPI"
 * database drivers that returns rows and columns.
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
#include <opencog/persist/tlb/TLB.h>

#include "llapi.h"
#include "SQLAtomStorage.h"

using namespace opencog;

/**
 * Utility class, hangs on to a single response to an SQL query, and
 * provides routines to parse it, i.e. walk the rows and columns,
 * converting each row into an Atom.
 *
 * Intended to be allocated on stack, to avoid malloc overhead.
 * Methods are intended to be inlined, so as to avoid subroutine
 * call overhead.  It's supposed to be a fast convenience wrapper.
 */
class SQLAtomStorage::Response
{
	public:
		LLRecordSet *rs;

		// Temporary cache of info about atom being assembled.
		UUID uuid;
		Type itype;
		const char* name;
		const char* outlist;
		int height;

		// Values
		double *floatval;
		const char *stringval;
		UUID *linkval;

	private:
		concurrent_stack<LLConnection*>& _pool;
		LLConnection* _conn;

	public:
		Response(concurrent_stack<LLConnection*>& pool) :
		    rs(nullptr),
		    itype(0),
		    name(nullptr),
		    outlist(nullptr),
		    height(0),
		    floatval(0),
		    stringval(nullptr),
		    linkval(nullptr),
		    _pool(pool),
		    _conn(nullptr),
		    table(nullptr),
		    store(nullptr),
		    pvec(nullptr),
		    uvec(nullptr),
		    tname(""),
		    fltval(0),
		    strval(nullptr),
		    lnkval(nullptr),
		    intval(0)
		{}

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
			if (nullptr == _conn) _conn = _pool.value_pop();
			rs = _conn->exec(buff, false);
		}
		void try_exec(const char * buff)
		{
			if (rs) rs->release();
			if (nullptr == _conn) _conn = _pool.value_pop();
			rs = _conn->exec(buff, true);
		}
		void exec(const std::string& str)
		{
			exec(str.c_str());
		}
		void try_exec(const std::string& str)
		{
			try_exec(str.c_str());
		}

		// Fetching of atoms -----------------------------------------
		bool create_atom_column_cb(const char *colname, const char * colvalue)
		{
			// printf ("%s = %s\n", colname, colvalue);
			// if (!strcmp(colname, "type"))
			if ('t' == colname[0])
			{
				itype = atoi(colvalue);
			}
			// else if (!strcmp(colname, "name"))
			else if ('n' == colname[0])
			{
				name = colvalue;
			}
			// else if (!strcmp(colname, "outgoing"))
			else if ('o' == colname[0])
			{
				outlist = colvalue;
			}
			// else if (!strcmp(colname, "uuid"))
			else if ('u' == colname[0])
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

		AtomSpace *table;
		SQLAtomStorage *store;
		bool load_all_atoms_cb(void)
		{
			// printf ("---- New atom found ----\n");
			rs->foreach_column(&Response::create_atom_column_cb, this);

			// Two different throws mighht be caught here:
			// 1) DB has an atom type that is not defined in the atomspace.
			//    In this case, makeAtom throws IOException.
			// 2) Corrupted databases can cause get_recursive_if_not_exists
			//    to throw, because a uuid does not exist. Yes, this can
			//    happen.
			// Either way, skip the offending atom, and carry on.
			try
			{
				PseudoPtr p(store->makeAtom(*this, uuid));

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
				h = store->get_recursive_if_not_exists(p);
				h = table->add(h, false);
				store->_tlbuf.addAtom(h, uuid);
			}
			else
			{
				// In case it's still in the TLB, but was
				// previously removed from the atomspace.
				h = table->add(h, false);
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

		// Fetching of uuids (for atom deletion) -----------------------
		bool get_uuid_column_cb(const char *colname, const char * colvalue)
		{
			// The column name will be either "uuid" or "key".
			// Since there will be only one column,
			// don't bother checking the column name...
			uuid = strtoul(colvalue, NULL, 10);
			return false;
		}

		std::vector<UUID> *uvec;
		bool get_uuid_cb(void)
		{
			rs->foreach_column(&Response::get_uuid_column_cb, this);

			uvec->emplace_back(uuid);
			return false;
		}

		// Types ------------------------------------------
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
			// if (!strcmp(colname, "floatvalue"))
			if ('f' == colname[0])
			{
				fltval = colvalue;
			}
			// else if (!strcmp(colname, "stringvalue"))
			else if ('s' == colname[0])
			{
				strval = colvalue;
			}
			// else if (!strcmp(colname, "linkvalue"))
			else if ('l' == colname[0])
			{
				lnkval = colvalue;
			}
			// else if (!strcmp(colname, "type"))
			else if ('t' == colname[0])
			{
				vtype = atoi(colvalue);
			}
			// else if (!strcmp(colname, "key"))
			else if ('k' == colname[0])
			{
				key = atol(colvalue);
			}
			// else if (!strcmp(colname, "atom"))
			else if ('a' == colname[0])
			{
				uuid = atol(colvalue);
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

				// Try really hard to stick the key into a table.
				// XXX This is potentially broken, as no other code
				// ever verifies that the key gets inserted into some
				// table.  The correct fix is to add AtomSpace as a
				// part of the BackingStore API. XXX TODO FIXME.
				if (table) hkey = table->add(hkey, false);
				else if (atom->getAtomSpace())
					hkey = atom->getAtomSpace()->add(hkey, false);
				store->_tlbuf.addAtom(hkey, key);
			}

			// The below usually triggers only on tvpred,
			// and so we could save some CPU cycles by handling
			// tvpred earlier, and avoiding this check.
			if (nullptr == hkey->getAtomSpace() and
			    nullptr != atom->getAtomSpace())
			{
				hkey = atom->getAtomSpace()->add(hkey, false);
				store->_tlbuf.addAtom(hkey, key);
			}

			ValuePtr pap = store->doUnpackValue(*this);
			atom->setValue(hkey, pap);
			return false;
		}

		// Generic things --------------------------------------------
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

/* ============================= END OF FILE ================= */
