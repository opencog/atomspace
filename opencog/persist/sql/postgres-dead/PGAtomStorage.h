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

#ifndef _OPENCOG_PG_ATOM_STORAGE_H
#define _OPENCOG_PG_ATOM_STORAGE_H

#include <atomic>
#include <mutex>
#include <set>
#include <thread>
#include <vector>

#include <opencog/util/async_method_caller.h>

#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/proto/types.h>

#include <opencog/atomspace/AtomTable.h>

#include <opencog/persist/sql/AtomStorage.h>
#include <opencog/persist/sql/postgres/odbcxx.h>

namespace opencog
{
/** \addtogroup grp_persist
 *  @{
 */

class MT19937RandGen;

class PGAtomStorage : public AtomStorage
{
    private:
        // Pool of shared connections
        ODBCConnection* get_conn();
        void put_conn(ODBCConnection*);
        concurrent_stack<ODBCConnection*> _conn_pool;

        // Utility for handling responses on stack.
        class Database;
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

        PseudoPtr make_pseudo_atom(Database&, UUID);
        PseudoPtr load_pseudo_atom(const char *, int);
        PseudoPtr load_pseudo_atom_with_uuid(UUID);

        // Atom height
        int get_height(AtomPtr);

        // Maximum atom height - stored in Globals table and cached here.
        int max_height;
        void store_max_height_global(int);
        int load_max_height_global();

        // Does a SELECT MAX(height) FROM Atoms table. Used to set the
        // globals table value after bulk stores.
        int load_max_atoms_height(void);

        // Helpers for storing calls
        void add_truth_value_columns(Database& database,
                                     AtomPtr atom);
        std::string build_atom_insert(Database& database,
                                      AtomPtr atom,
                                      int height,
                                      int out_differentiator = 0);
        std::string build_atom_update(Database& database,
                                      AtomPtr atom);

        // The actual storing calls.
        int do_store_atom_recursive(Database&, AtomPtr);
        void vdo_store_atom(const AtomPtr&);
        void do_store_atom_single(Database&, AtomPtr, int);

        std::string outgoing_set_to_string(const HandleSeq&);
        std::string outgoing_set_to_hash_string(const HandleSeq&);
        void cache_edges_where(const char * where_clause);
        void store_outgoing_edges(AtomPtr);
        void get_outgoing_edges(UUID uuid, std::vector<UUID>&);
        int load_max_hash_differentiator(Type t, 
                                         const HandleSeq& outgoing);
        bool outgoing_matches_uuids(const HandleSeq& handles,
                                    std::vector<UUID> uuids);

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

        UUID load_max_atoms_uuid(void);
        bool query_uuid_exists(UUID uuid);

        // Reserve a range of UUID's with TLB that includes the
        // maximum found in Atoms. This is called in init() when
        // the database is first connected.
        UUID reserve_max_atoms_uuid(void);     

        // The type_map translates between opencog type numbers and
        // the database type numbers.  Initially, they match up, but
        // might get askew if new types are added or deleted.

        // TYPEMAP_SZ is defined as the maximum number of possible
        // OpenCog Types (65536 as Type is currently a short int)
        static_assert(2 == sizeof(Type),
             "*** Typemap needs to be redesigned to handle larger types! ***");
        #define TYPEMAP_SZ (1 << (8 * sizeof(Type)))
        int _storing_type_map[TYPEMAP_SZ];
        Type _loading_type_map[TYPEMAP_SZ];
        char * _database_type_names[TYPEMAP_SZ];

        bool type_map_was_loaded;
        void load_type_map(void);
        void setup_type_map(void);
        void map_database_type(int, const char *);

        // Provider of asynchronous store of atoms.
        async_caller<PGAtomStorage, AtomPtr> _write_queue;

        bool _verbose;
        bool _print_statements;
        bool _store_edges;
        bool _generate_collisions;
        int _collission_count;
        MT19937RandGen* _random_generator;
        int _query_count;

        int _transaction_chunk;
        uint64_t _hash_seed;

        std::unordered_map<UUID, std::vector<UUID>> _edge_cache;

        // Atom Caching for getAtom optimization...
        std::unordered_map<UUID, AtomPtr> _atom_cache;
        void cache_atom(UUID uuid, AtomPtr atom);
        AtomPtr get_cached_atom(UUID uuid);

    public:
        PGAtomStorage(const std::string& dbname, 
                    const std::string& username,
                    const std::string& authentication);
        PGAtomStorage(const char * dbname, 
                    const char * username,
                    const char * authentication);
        PGAtomStorage(const PGAtomStorage&) = delete; // disable copying
        PGAtomStorage& operator=(const PGAtomStorage&) = delete; // disable assignment
        virtual ~PGAtomStorage();
        bool connected(void); // connection to DB is alive

        void kill_data(void); // destroy DB contents

        // Testing helpers.
        bool verbose()
            { return _verbose; }
        void setVerbose()
            { _verbose = true; }
        void setVerboseOff()
            { _verbose = false; }

        bool printStatements()
            { return _print_statements; }
        void setPrintStatements()
            { _print_statements = true; }
        void setPrintStatementsOff()
            { _print_statements = false; }

        bool storingEdges()
            { return _store_edges; }
        void setStoreEdges()
            { _store_edges = true; }
        void setDontStoreEdges()
            { _store_edges = false; }

        void resetQueryCount()
            { _query_count = 0; }
        int queryCount()
            { return _query_count; }

        void setTransactionChunk(int transaction_chunk)
            { _transaction_chunk = transaction_chunk; }

        // Enable stress tests and output suitable for testing.
        // Among other things, this will generate collisions for
        // a consistent set of outgoing set hashs to test the
        // handlings of collisions.
        void enable_testing_mode();
        void disable_testing_mode();

        // AtomStorage interface
        TruthValuePtr getNode(Type, const char *);
        TruthValuePtr getLink(const Handle&);
        HandleSeq getIncomingSet(const Handle&);
        void storeAtom(const AtomPtr&, bool synchronous = false);
        void loadType(AtomTable &, Type);
        void flushStoreQueue();

        // Large-scale loads and saves
        void load(AtomTable &); // Load entire contents of DB
        void store(const AtomTable &); // Store entire contents of AtomTable
};


/** @}*/
} // namespace opencog

#endif // _OPENCOG_PG_ATOM_STORAGE_H
