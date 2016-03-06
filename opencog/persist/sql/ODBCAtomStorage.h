/*
 * FUNCTION:
 * Base class for SQL-backed persistent storage.
 *
 * HISTORY:
 * Copyright (c) 2008,2009 Linas Vepstas <linasvepstas@gmail.com>
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

#ifndef _OPENCOG_ODBC_ATOM_STORAGE_H
#define _OPENCOG_ODBC_ATOM_STORAGE_H

#include <atomic>
#include <mutex>
#include <set>
#include <thread>
#include <vector>

#include <opencog/util/async_method_caller.h>

#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/base/types.h>

#include <opencog/atomspace/AtomTable.h>

#include <opencog/persist/sql/odbcxx.h>
#include <opencog/persist/sql/AtomStorage.h>

namespace opencog
{
/** \addtogroup grp_persist
 *  @{
 */

class ODBCAtomStorage : public AtomStorage
{
	private:
		// Pool of shared connections
		ODBCConnection* get_conn();
		void put_conn(ODBCConnection*);
		concurrent_stack<ODBCConnection*> conn_pool;

		// Utility for handling responses on stack.
		class Response;
		class Outgoing;

		void init(const char *, const char *, const char *);

		// ---------------------------------------------
		// Handle multiple atomspaces like typecodes: we have to
		// convert from sql UUID to the atual UUID.
		std::mutex table_cache_mutex;
		bool table_cache_is_inited;
		std::set<UUID> table_id_cache;
		void store_atomtable_id(const AtomTable&);

		// ---------------------------------------------
		struct PseudoAtom
			: public std::enable_shared_from_this<PseudoAtom>
		{
			Type type;
			UUID uuid;
			std::string name;
			std::vector<UUID> oset;
			TruthValuePtr tv;
		};
		typedef std::shared_ptr<PseudoAtom> PseudoPtr;
		#define createPseudo std::make_shared<PseudoAtom>
		PseudoPtr makeAtom(Response &, UUID);
		PseudoPtr getAtom(const char *, int);
		PseudoPtr petAtom(UUID);

		int get_height(AtomPtr);
		int max_height;
		void setMaxHeight(int);
		int getMaxHeight(void);

		int do_store_atom(AtomPtr);
		void vdo_store_atom(AtomPtr&);
		void do_store_single_atom(AtomPtr, int);

		std::string oset_to_string(const HandleSeq&, int);
		void storeOutgoing(AtomPtr, Handle);
		void getOutgoing(HandleSeq&, Handle);
		bool store_cb(AtomPtr);
		std::atomic<unsigned long> load_count;
		std::atomic<unsigned long> store_count;

		void rename_tables(void);
		void create_tables(void);

		// Track UUID's that are in use.
		std::mutex id_cache_mutex;
		bool local_id_cache_is_inited;
		std::set<UUID> local_id_cache;
		void add_id_to_cache(UUID);
		void get_ids(void);

		std::mutex id_create_mutex;
		std::set<UUID> id_create_cache;
		std::unique_lock<std::mutex> maybe_create_id(UUID);

		UUID getMaxObservedUUID(void);
		int getMaxObservedHeight(void);
		bool idExists(const char *);

		// The typemap translates between opencog type numbers and
		// the database type numbers.  Initially, they match up, but
		// might get askew if new types are added or deleted.

		// TYPEMAP_SZ is defined as the maximum number of possible
		// OpenCog Types (65536 as Type is currently a short int)
		static_assert(2 == sizeof(Type),
			 "*** Typemap needs to be redesigned to handle larger types! ***");
		#define TYPEMAP_SZ (1 << (8 * sizeof(Type)))
		int storing_typemap[TYPEMAP_SZ];
		Type loading_typemap[TYPEMAP_SZ];
		char * db_typename[TYPEMAP_SZ];

		bool type_map_was_loaded;
		void load_typemap(void);
		void setup_typemap(void);
		void set_typemap(int, const char *);

#ifdef OUT_OF_LINE_TVS
		bool tvExists(int);
		int storeTruthValue(AtomPtr, Handle);
		int  TVID(const TruthValue &);
		TruthValue * getTV(int);
#endif /* OUT_OF_LINE_TVS */

		// Provider of asynchronous store of atoms.
		async_caller<ODBCAtomStorage, AtomPtr> _write_queue;

	public:
		ODBCAtomStorage(const std::string& dbname, 
		            const std::string& username,
		            const std::string& authentication);
		ODBCAtomStorage(const char * dbname, 
		            const char * username,
		            const char * authentication);
		ODBCAtomStorage(const ODBCAtomStorage&) = delete; // disable copying
		ODBCAtomStorage& operator=(const ODBCAtomStorage&) = delete; // disable assignment
		virtual ~ODBCAtomStorage();
		bool connected(void); // connection to DB is alive

		void kill_data(void); // destroy DB contents

		// AtomStorage interface
		NodePtr getNode(Type, const char *);
		LinkPtr getLink(Type, const std::vector<Handle>&);
		AtomPtr getAtom(UUID);
		std::vector<Handle> getIncomingSet(Handle);
		void storeAtom(AtomPtr atomPtr, bool synchronous = false);
		void loadType(AtomTable &, Type);
		void flushStoreQueue();

		// Store atoms to DB
		void storeSingleAtom(AtomPtr);

		// Fetch atoms from DB
		bool atomExists(Handle);
		AtomPtr getAtom(const Handle& h)
		{
			NodePtr n(NodeCast(h));
			if (n) return getNode(*n);
			LinkPtr l(LinkCast(h));
			if (l) return getLink(*l);
			return NULL;
		}
		NodePtr getNode(const Node &n)
		{
			return getNode(n.getType(), n.getName().c_str());
		}
		LinkPtr getLink(const Link &l)
		{
			return getLink(l.getType(), l.getOutgoingSet());
		}

		// Large-scale loads and saves
		void load(AtomTable &); // Load entire contents of DB
		void store(const AtomTable &); // Store entire contents of AtomTable
		void reserve(void);     // reserve range of UUID's
};


/** @}*/
} // namespace opencog

#endif // _OPENCOG_ODBC_ATOM_STORAGE_H
