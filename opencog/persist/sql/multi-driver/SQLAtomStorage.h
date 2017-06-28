/*
 * FUNCTION:
 * SQL-backed persistent storage.
 *
 * HISTORY:
 * Copyright (c) 2008,2009,2017 Linas Vepstas <linasvepstas@gmail.com>
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

#ifndef _OPENCOG_SQL_ATOM_STORAGE_H
#define _OPENCOG_SQL_ATOM_STORAGE_H

#include <atomic>
#include <mutex>
#include <set>
#include <thread>
#include <vector>

// #include <opencog/util/async_method_caller.h>
#include <opencog/util/async_buffer.h>

#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/base/types.h>
#include <opencog/atoms/base/FloatValue.h>
#include <opencog/atoms/base/LinkValue.h>
#include <opencog/atoms/base/StringValue.h>
#include <opencog/atoms/base/Valuation.h>

#include <opencog/atomspace/AtomTable.h>
#include <opencog/atomspaceutils/TLB.h>
#include <opencog/persist/sql/AtomStorage.h>

#include "llapi.h"

namespace opencog
{
/** \addtogroup grp_persist
 *  @{
 */

/// This class can only be used safely as a singleton; however, this
/// singleton can be used by multiple threads.
class SQLAtomStorage : public AtomStorage
{
	private:
		// Pool of shared connections
		concurrent_stack<LLConnection*> conn_pool;
		int _initial_conn_pool_size;

		// Utility for handling responses (on stack).
		class Response;
		class Outgoing;

		void init(const char *);
		std::string _uri;

		// ---------------------------------------------
		// Handle multiple atomspaces like typecodes: we have to
		// convert from sql UUID to the actual UUID.
		std::set<UUID> table_id_cache;
		void store_atomtable_id(const AtomTable&);

		// ---------------------------------------------
		// Fetching of atoms.
		struct PseudoAtom
		    : public std::enable_shared_from_this<PseudoAtom>
		{
		    Type type;
		    UUID uuid;
		    std::string name;
		    std::vector<UUID> oset;
		};
		typedef std::shared_ptr<PseudoAtom> PseudoPtr;
		#define createPseudo std::make_shared<PseudoAtom>
		PseudoPtr makeAtom(Response &, UUID);
		PseudoPtr getAtom(const char *, int);
		PseudoPtr petAtom(UUID);

		Handle get_recursive_if_not_exists(PseudoPtr);

		Handle doGetNode(Type, const char *);
		Handle doGetLink(Type, const HandleSeq&);

		int get_height(const Handle&);
		int max_height;

		void getIncoming(AtomTable&, const char *);
		// --------------------------
		// Storing of atoms
		std::mutex _store_mutex;

		int do_store_atom(const Handle&);
		void vdo_store_atom(const Handle&);
		void do_store_single_atom(const Handle&, int);

		bool not_yet_stored(const Handle&);
		UUID check_uuid(const Handle&);
		UUID get_uuid(const Handle&);
		std::string oset_to_string(const HandleSeq&);

		bool bulk_load;
		bool bulk_store;
		time_t bulk_start;

		// --------------------------
		// Table management
		void rename_tables(void);
		void create_tables(void);

		// --------------------------
		// UUID management
		UUID getMaxObservedUUID(void);
		int getMaxObservedHeight(void);
		TLB _tlbuf;

		// --------------------------
		// Values
#define NUMVMUT 16
		std::mutex _value_mutex[NUMVMUT];
		void store_atom_values(const Handle &);
		void get_atom_values(Handle &);

		typedef unsigned long VUID;

		ProtoAtomPtr doUnpackValue(Response&);
		ProtoAtomPtr doGetValue(const char *);

		VUID storeValue(const ProtoAtomPtr&);
		ProtoAtomPtr getValue(VUID);
		void deleteValue(VUID);

		VUID getMaxObservedVUID(void);
		std::atomic<VUID> _next_valid;

		// --------------------------
		// Valuations
		std::mutex _valuation_mutex;
		void storeValuation(const ValuationPtr&);
		void storeValuation(const Handle&, const Handle&, const ProtoAtomPtr&);
		ProtoAtomPtr getValuation(const Handle&, const Handle&);
		void deleteValuation(const Handle&, const Handle&);

		std::string float_to_string(const FloatValuePtr&);
		std::string string_to_string(const StringValuePtr&);
		std::string link_to_string(const LinkValuePtr&);

		Handle tvpred; // the key to a very special valuation.
		// --------------------------
		// Performance statistics
		std::atomic<size_t> _num_get_nodes;
		std::atomic<size_t> _num_got_nodes;
		std::atomic<size_t> _num_rec_nodes;
		std::atomic<size_t> _num_get_links;
		std::atomic<size_t> _num_got_links;
		std::atomic<size_t> _num_rec_links;
		std::atomic<size_t> _num_get_insets;
		std::atomic<size_t> _num_get_inlinks;
		std::atomic<size_t> _num_node_inserts;
		std::atomic<size_t> _num_link_inserts;
		std::atomic<size_t> _load_count;
		std::atomic<size_t> _store_count;
		std::atomic<size_t> _valuation_stores;
		std::atomic<size_t> _value_stores;
		time_t _stats_time;

		// -------------------------------
		// Type management
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
		std::mutex _typemap_mutex;

		// Provider of asynchronous store of atoms.
		// async_caller<SQLAtomStorage, Handle> _write_queue;
		async_buffer<SQLAtomStorage, Handle> _write_queue;

	public:
		SQLAtomStorage(std::string uri);
		SQLAtomStorage(const SQLAtomStorage&) = delete; // disable copying
		SQLAtomStorage& operator=(const SQLAtomStorage&) = delete; // disable assignment
		virtual ~SQLAtomStorage();
		bool connected(void); // connection to DB is alive

		void kill_data(void); // destroy DB contents
		void clear_cache(void); // clear out the TLB.

		void registerWith(AtomSpace*);
		void unregisterWith(AtomSpace*);
		void extract_callback(const AtomPtr&);
		boost::signals2::connection _extract_sig;

		// AtomStorage interface
		Handle getNode(Type, const char *);
		Handle getLink(Type, const HandleSeq&);
		void getIncomingSet(AtomTable&, const Handle&);
		void getIncomingByType(AtomTable&, const Handle&, Type t);
		void storeAtom(const Handle&, bool synchronous = false);
		void loadType(AtomTable&, Type);
		void flushStoreQueue();

		// Large-scale loads and saves
		void load(AtomTable &); // Load entire contents of DB
		void store(const AtomTable &); // Store entire contents of AtomTable
		void reserve(void);     // reserve range of UUID's

		// Debugging and performance monitoring
		void print_stats(void);
		void clear_stats(void); // reset stats counters.
		void set_hilo_watermarks(int, int);
		void set_stall_writers(bool);
};


/** @}*/
} // namespace opencog

#endif // _OPENCOG_SQL_ATOM_STORAGE_H
