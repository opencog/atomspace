/*
 * FUNCTION:
 * Persistent Atom storage, PostgreSQL SQL-backed.
 *
 * Atoms are saved to, and restored from, an SQL DB using the ODBC driver.
 * Atoms are identified by means of unique ID's, which are taken to
 * be the atom Handles, as maintained by the TLB. In particular, the
 * system here depends on the handles in the TLB and in the SQL DB
 * to be consistent (i.e. kept in sync).
 *
 * Copyright (c) 2008,2009,2013 Linas Vepstas <linas@linas.org>
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
#include <opencog/util/random.h>
#include <opencog/util/oc_assert.h>
#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/proto/NameServer.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/truthvalue/CountTruthValue.h>
#include <opencog/truthvalue/IndefiniteTruthValue.h>
#include <opencog/truthvalue/ProbabilisticTruthValue.h>
#include <opencog/truthvalue/SimpleTruthValue.h>
#include <opencog/truthvalue/TruthValue.h>
#include <opencog/atomspace/TypeIndex.h>
#include <opencog/atomspaceutils/TLB.h>
#include <opencog/persist/sql/postgres/odbcxx.h>
#include <opencog/persist/sql/postgres/OutgoingHash.h>

#include "PGAtomStorage.h"

using namespace opencog;

// The default connection pool size.
#define CONNECTION_POOL_SIZE 12

// How often to report loading and storing of atoms to stdout.
#define REPORTING_INTERVAL  10000

// Chunk size for limiting Atom selects.
#define LOAD_CHUNK 10000

// Chunk size for writes to Edges table.
#define EDGE_CHUNK 10000

// Normal edge cache maximum. We use more for bulk loading but for the
// normal case we only need to cache for a full depth get.
#define EDGE_CACHE_MAX_SIZE 1000

// Atom cache for handling nested atoms. Set to the expected number of
// atoms to be retrieved as a result of a single get operation of a
// parent link.
#define ATOM_CACHE_SIZE 1000

// Initial growth for outgoing sets
#define INITIAL_GROWTH_CHUNK 8

#define NO_UUID UINT64_MAX

#define BUFFER_SIZE 250

// Column MAX limits
#define MAX_NODE_NAME_LENGTH 2700

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
class PGAtomStorage::Database
{
public:
    ODBCRecordSet *_result_set;
    ODBCConnection* _db_connection;

    // Temporary cache of info about atom being assembled.
    UUID uuid;
    int itype;
    const char * name;
    int tv_type;
    double mean;
    double confidence;
    double count;
    const char *outlist;
    int differentiator;
    int height;

    bool _inserting;
    std::string _table;
    std::vector<std::string> _columns;
    std::vector<std::string> _values;

    AtomTable *_atom_table;
    PGAtomStorage *_atom_storage;

    Database(PGAtomStorage * atom_store, AtomTable* atom_table = NULL)
    {
        _atom_storage = atom_store;
        _atom_table = atom_table;
        init();

        // Grab a connection from the pool.
        _db_connection = _atom_storage->_conn_pool.pop();
    }

    ~Database()
    {
        // Release the result set if it was used and reset the variables.
        if (_result_set)
            _result_set->release();

        // Put the connection back into the pool.
        _atom_storage->_conn_pool.push(_db_connection);
    }

    void init()
    {
        type_name = "";
        itype = 0;
        _result_set = NULL;
        _inserting = false;
    }

    void execute(const char* statement)
    {
        // Release the existing result set if we have one, and reset the
        // statement variables. That way you can call execute multiple
        // times without having to worry about memory leaking result sets.
        if (_result_set)
        {
            _result_set->release();
            init();
        }

        // Print the statement.
        if (_atom_storage->printStatements())
        {
            fprintf(stdout, "%5d - %s\n", _atom_storage->_query_count, 
                    statement);
        }

        // Increment the query count.
        if (_atom_storage)
            _atom_storage->_query_count++;

        // Execute the statement and store the result set.
        _result_set = _db_connection->exec(statement);

        // Sometimes there are no results and _result_set will be NULL
        // even though there is no error.
    }

    void for_each_column(bool (Database::*callback)(const char *,
                                                    const char *))
    {
        if (_result_set)
            _result_set->foreach_column(callback, this);
    }


    void for_each_row(bool (Database::*callback)(void))
    {
        if (_result_set)
            _result_set->foreach_row(callback, this);
    }

    unsigned long get_int_result()
    {
        // Get the result row as an unsigned long and return it.
        _result_set->fetch_row();
        const char* column_chars = _result_set->get_column_value(0);
        return strtoul(column_chars, NULL, 10);
    }

    void start_insert(const char* table)
    {
        _table = table;
        _inserting = true;
        _columns.clear();
        _values.clear();
    }

    void start_update(const char* table)
    {
        _table = table;
        _inserting = false;
        _columns.clear();
        _values.clear();
    }

    void add_column_quoted_string(const char* column, 
                                  const std::string& value,
                                  unsigned max_length = 0)
    {
        add_column_quoted_string(column, value.c_str(), max_length);
    }

    void add_column_quoted_string(const char* column, 
                                  const char* value,
                                  unsigned max_length = 0)
    {
        if (max_length and max_length < strlen(value))
        {
            throw RuntimeException(TRACE_INFO,
                    "add_column - maximum length for %s column %s[%d] "
                    " exceeded by value %s",
                    _table.c_str(), column, max_length, value);
        }

        // Use postgres $-quoting to make unicode strings
        // easier to deal with.
        std::string quoted_value = "$ocp$";
        quoted_value += value;
        quoted_value += "$ocp$";

        // Add the quoted value.
        _columns.emplace_back(std::string(column));
        _values.emplace_back(quoted_value);
    }

    void add_column_string(const char* column, 
                           const std::string& value,
                           unsigned max_length = 0)
    {
        add_column_string(column, value.c_str(), max_length);
    }

    void add_column_string(const char* column, 
                           const char* value,
                           unsigned max_length = 0)
    {
        // Check the max length against the string.
        if (max_length and max_length < strlen(value))
        {
            throw RuntimeException(TRACE_INFO,
                    "add_column - maximum length for %s column %s[%d] "
                    " exceeded by value %s",
                    _table.c_str(), column, max_length, value);
        }

        // Add the value.
        _columns.emplace_back(std::string(column));
        _values.emplace_back(std::string(value));
    }

    void add_column_unsigned(const char* column, unsigned value)
    {
        _columns.emplace_back(std::string(column));
        char value_chars[BUFFER_SIZE];
        snprintf(value_chars, BUFFER_SIZE, "%u", value);
        _values.emplace_back(std::string(value_chars));
    }

    void add_column_bigint(const char* column, int64_t value)
    {
        _columns.emplace_back(std::string(column));
        _values.emplace_back(std::to_string(value));
    }

    void add_column_double(const char* column, double value)
    {
        _columns.emplace_back(std::string(column));
        char value_chars[BUFFER_SIZE];
        snprintf(value_chars, BUFFER_SIZE, "%12.8g", value);
        _values.emplace_back(std::string(value_chars));
    }

    std::string build_insert_statement()
    {
        // Add the insert header.
        std::string statement = "INSERT INTO ";
        statement += _table;
        statement += " (";

        // Add the column names.
        int column_index = 0;
        for (auto& column : _columns)
        {
            if (column_index > 0)
                statement += ", ";
            statement += column;
            column_index++;
        }

        // Now add the values.
        statement += ") VALUES (";
        column_index = 0;
        for (auto& value : _values)
        {
            if (column_index > 0)
                statement += ", ";
            statement += value;
            column_index++;
        }

        // Close up the statement and return.
        statement += ");";
        return statement;
    }

    std::string build_update_where(const char* where_clause)
    {
        // Add the update header.
        std::string statement = "UPDATE ";
        statement += _table;
        statement += " SET ";

        // Now loop over all the column value pairs adding each
        // to the SET clause.
        int column_index = 0;
        for (auto& column : _columns)
        {
            // Add the comma after the first column.
            if (column_index > 0)
                statement += ", ";

            // Set "column = value" for this pair.
            statement += column;
            statement += " = ";
            statement += _values[column_index];
            column_index++;
        }

        // Close up the statement and return.
        statement += ";";
        return statement;
    }

    std::string build_update_where(const char* column, int64_t key)
    {
        // Build the where clause from the supplied column and key.
        std::string where_clause = " WHERE ";
        where_clause += column;
        where_clause += " = ";
        where_clause += std::to_string(key);
        where_clause += ";";

        // Defer to the more general update statement build.
        return build_update_where(where_clause.c_str());
    }

    bool has_results()
    {
        return (_result_set != NULL);
    }

    int column_count()
    {
        return _result_set->get_column_count();
    }

    const char* column_value(int column)
    {
        return _result_set->get_column_value(column);
    }

    int fetch_next_row()
    {
        if (_result_set)
            return _result_set->fetch_row();
        else
            return 0;
    }

    // Begin Callbacks

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
        else if (not _atom_storage->_store_edges and 
                 !strcmp(colname, "outgoing"))
        {
            outlist = colvalue;
        }
        else if (_atom_storage->_store_edges and 
                 !strcmp(colname, "out_differentiator"))
        {
            differentiator = atoi(colvalue);
        }
        else if (!strcmp(colname, "tv_type"))
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
    int row_count;
    bool create_atom_cb(void)
    {
        // printf ("---- New atom found ----\n");
        _result_set->foreach_column(&Database::create_atom_column_cb, this);
        row_count++;
        return false;
    }

    bool load_all_atoms_cb(void)
    {
        // printf ("---- New atom found ----\n");
        _result_set->foreach_column(&Database::create_atom_column_cb, this);

        PseudoPtr p(_atom_storage->make_pseudo_atom(*this, uuid));
        AtomPtr atom(get_recursive_if_not_exists(p));
        _atom_table->add(atom, true);
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
        _result_set->foreach_column(&Database::create_atom_column_cb, this);

        Handle h(TLB::getAtom(uuid));
        if (nullptr == _atom_table->getHandle(h))
        {
            PseudoPtr p(_atom_storage->make_pseudo_atom(*this, uuid));
            AtomPtr atom(get_recursive_if_not_exists(p));
            _atom_table->add(atom, true);
        }
        return false;
    }

    HandleSeq *hvec;
    bool fetch_incoming_set_cb(void)
    {
        // printf ("---- New atom found ----\n");
        _result_set->foreach_column(&Database::create_atom_column_cb, this);

        // Note, unlike the above 'load' routines, this merely fetches
        // the atoms, and returns a vector of them.  They are loaded
        // into the atomspace later, by the caller.
        PseudoPtr p(_atom_storage->make_pseudo_atom(*this, uuid));
        AtomPtr atom(get_recursive_if_not_exists(p));
        hvec->emplace_back(atom->get_handle());
        return false;
    }

    // Helper function for above.  The problem is that, when
    // adding links of unknown provenance, it could happen that
    // the outgoing set of the link has not yet been loaded.  In
    // that case, we have to load the outgoing set first.
    AtomPtr get_recursive_if_not_exists(PseudoPtr p)
    {
        if (nameserver().isA(p->type, NODE))
        {
            NodePtr node(createNode(p->type, p->name, p->tv));
            TLB::addAtom(node, p->uuid);
            return node;
        }
        HandleSeq resolved_oset;
        for (UUID idu : p->oset)
        {
            Handle h(TLB::getAtom(idu));
            h = _atom_table->getHandle(h);
            if (h)
            {
                resolved_oset.emplace_back(h);
                continue;
            }
            PseudoPtr po = _atom_storage->load_pseudo_atom_with_uuid(idu);
            AtomPtr ra = get_recursive_if_not_exists(po);
            resolved_oset.emplace_back(ra->get_handle());
        }
        LinkPtr link(createLink(p->type, resolved_oset, p->tv));
        TLB::addAtom(link, p->uuid);
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
        _result_set->foreach_column(&Database::type_column_cb, this);
        _atom_storage->map_database_type(itype, type_name);
        return false;
    }
    const char * type_name;
    bool type_column_cb(const char *colname, const char * colvalue)
    {
        if (!strcmp(colname, "type"))
        {
            itype = atoi(colvalue);
        }
        else if (!strcmp(colname, "typename"))
        {
            type_name = colvalue;
        }
        return false;
    }

    // Get all handles in the database.
    std::set<UUID> *id_set;
    bool note_id_cb(void)
    {
        _result_set->foreach_column(&Database::note_id_column_cb, this);
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
// Constructors

void PGAtomStorage::init(const char * dbname,
                         const char * username,
                         const char * authentication)
{
#ifdef DEBUG
    _verbose = true;
    _print_statements = true;
#else
    _verbose = false;
    _print_statements = false;
#endif
    _store_edges = true;
    _query_count = 0;
    _generate_collisions = false;
    _collission_count = 0;
    _transaction_chunk = 0;
    _hash_seed = 0x38aa725897239ecf;

    // Generate random numbers using our hash seed.
    _random_generator = new MT19937RandGen(_hash_seed);

    // int random_integer = _random_generator->randint(100);

    // Create the connection pool.
    for (int i=0; i < CONNECTION_POOL_SIZE; i++)
    {
        ODBCConnection* db_conn = new ODBCConnection(dbname, username,
                authentication);
        _conn_pool.push(db_conn);
    }
    type_map_was_loaded = false;
    max_height = 0;

    for (int i=0; i< TYPEMAP_SZ; i++)
    {
        _database_type_names[i] = NULL;
    }

    local_id_cache_is_inited = false;
    table_cache_is_inited = false;
    if (!connected()) return;

    // Reserve a range of UUID's with TLB that includes the
    // maximum found in Atoms. 
    reserve_max_atoms_uuid();

    // Load the types and setup the type map.
    setup_type_map();

    // Reset the query count since all the priors were housekeeping. Unless
    // we were already printing statements, in which case, we'll keep the
    // numbers since they'll make sense in that context.
    if (_print_statements)
        _query_count = 0;
}

PGAtomStorage::PGAtomStorage(const char * dbname,
                         const char * username,
                         const char * authentication)
    : _write_queue(this, &PGAtomStorage::vdo_store_atom)
{
    init(dbname, username, authentication);
}

PGAtomStorage::PGAtomStorage(const std::string& dbname,
                         const std::string& username,
                         const std::string& authentication)
    : _write_queue(this, &PGAtomStorage::vdo_store_atom)
{
    init(dbname.c_str(), username.c_str(), authentication.c_str());
}

PGAtomStorage::~PGAtomStorage()
{
    if (_random_generator)
        delete _random_generator;

    if (connected())
        store_max_height_global(load_max_atoms_height());

    while (not _conn_pool.is_empty())
    {
        ODBCConnection* db_conn = _conn_pool.pop();
        delete db_conn;
    }

    for (int i=0; i<TYPEMAP_SZ; i++)
    {
        if (_database_type_names[i]) free(_database_type_names[i]);
    }
}

/* ================================================================ */

void PGAtomStorage::enable_testing_mode()
{
    _print_statements = false;
    _generate_collisions = true;
}

void PGAtomStorage::disable_testing_mode()
{
    _print_statements = false;
    _generate_collisions = false;
}

bool PGAtomStorage::query_uuid_exists(UUID uuid)
{
    Database database(this);
    char statement[BUFFER_SIZE];
    snprintf(statement, BUFFER_SIZE, "SELECT uuid FROM Atoms "
            "WHERE uuid = %lu;", uuid);

    // Check it it exists.
    database.row_exists = false;
    database.execute(statement);
    database.for_each_row(&Database::row_exists_cb);
    return database.row_exists;
}

/**
 * Store the outgoing set of the atom in the Edges table.
 */
void PGAtomStorage::store_outgoing_edges(AtomPtr atom)
{
    Database database(this);
    char insert[BUFFER_SIZE * EDGE_CHUNK];
    const char* separator = "";

    // Get the UUID for the atom.
    UUID source_uuid = TLB::addAtom(atom, TLB::INVALID_UUID);

    // Write the constant insert preamble which we'll append to
    // in the loop below.
    snprintf(insert, BUFFER_SIZE, "INSERT INTO Edges "
            "(src_uuid, dst_uuid, pos) VALUES ");
    unsigned int preamble_length = strlen(insert);

    // Loop over all the outgoing handles...
    unsigned int position = 0;
    unsigned int edge_chunk_count = 0;
    unsigned int insert_length = preamble_length;
    for (auto h : atom->getOutgoingSet())
    {
        // Write a single atom's Edges VALUES clause.
        UUID destination_uuid = TLB::addAtom(h, TLB::INVALID_UUID);
        snprintf(insert + insert_length, BUFFER_SIZE, 
                 "%s (%lu, %lu, %u)",
                separator, source_uuid, destination_uuid, position);
        insert_length += strlen(insert + insert_length);
        edge_chunk_count++;

        // Prepare for one more row.
        separator = ",";
        position++;

        // If we have a full chunk...
        if (edge_chunk_count % EDGE_CHUNK == 0)
        {
            // Add the insert terminator.
            strcat(insert + insert_length, ";");

            // Execute the insert.
            database.execute(insert);

            // Reset the edge chunk.
            edge_chunk_count = 0;
            insert_length = preamble_length;
            separator = "";
        }
    }

    // If there are still edges to write...
    if (edge_chunk_count > 0)
    {
        // Add the insert terminator.
        strcat(insert + insert_length, ";");

        // Execute the insert.
        database.execute(insert);
    }
}

/**
 * connected -- return true if a successful connection to the
 * database exists; else return false.  Note that this may block,
 * if all database connections are in use...
 */
bool PGAtomStorage::connected(void)
{
    ODBCConnection* db_conn = _conn_pool.pop();
    bool have_connection = db_conn->connected();
    _conn_pool.push(db_conn);
    return have_connection;
}

/* ================================================================== */
/* AtomTable UUID stuff */

void PGAtomStorage::store_atomtable_id(const AtomTable& at)
{
    Database database(this);
    UUID tab_id = at.get_uuid();
    if (table_id_cache.count(tab_id)) return;

    table_id_cache.insert(tab_id);

    // Get the parent table as well.
    UUID parent_id = 0;
    AtomTable *env = at.get_environ();
    if (env)
    {
        parent_id = env->get_uuid();
        store_atomtable_id(*env);
    }

    if (0 == parent_id and 0 == tab_id) return;

    char statement[BUFFER_SIZE];
    snprintf(statement, BUFFER_SIZE,
        "INSERT INTO Spaces (space, parent) VALUES (%ld, %ld);",
        tab_id, parent_id);

    std::unique_lock<std::mutex> lock(table_cache_mutex);
    database.execute(statement);
}


//* ================================================================== */

/**
 * Return largest distance from this atom to any node under it.
 * Nodes have a height of 0, by definition.  Links that contain only
 * nodes in their outgoing set have a height of 1, by definition.
 * The height of a link is, by definition, one more than the height
 * of the tallest atom in its outgoing set.
 * @note This can conversely be viewed as the depth of a tree.
 */
int PGAtomStorage::get_height(AtomPtr atom)
{
    LinkPtr l(LinkCast(atom));
    if (NULL == l) return 0;

    int maxd = 0;
    int arity = l->get_arity();

    const HandleSeq& out = l->getOutgoingSet();
    for (int i=0; i<arity; i++)
    {
        Handle h = out[i];
        int d = get_height(h);
        if (maxd < d) maxd = d;
    }
    return maxd +1;
}

/* ================================================================ */

std::string PGAtomStorage::outgoing_set_to_string(const HandleSeq& outgoing)
{
    std::string str;
    str += "\'{";
    bool first_atom = true;
    for (auto& atom : outgoing)
    {
        // Add the comma delimiter if this is not the first atom.
        if (not first_atom)
            str += ", ";

        // And the handle's UUID.
        if (atom == NULL)
        {
            throw RuntimeException(TRACE_INFO, "Fatal Error: PGAtomStorage::"
                    "outgoing_set_to_string - NULL handle in outgoing set\n");
        }
        
        // Add this atom's UUID.
        str += std::to_string(TLB::addAtom(atom, TLB::INVALID_UUID));

        // Not the first atom, so next time we'll add a comma.
        first_atom = false;
    }
    str += "}\'";
    // fprintf(stdout, "%s\n", str.c_str());
    return str;
}

/* ================================================================ */
#define COLLIDE_INTERVAL 10
std::string PGAtomStorage::outgoing_set_to_hash_string(const HandleSeq& outgoing)
{
    // Hash the outgoing set.
    int64_t hash = hash_outgoing(outgoing, _hash_seed);

    // If the hash matches our mask, then we'll fake a collision.
    // This is used during testing to test the collision handling.
    if (_generate_collisions)
    {
        if (hash % COLLIDE_INTERVAL == 0)
        {
            // We'll collide to something that uses all the bits but that
            // we can easily recognize like: 1010101010101010101 decimal
            int64_t new_hash = 0xe04998456557eb5;
            _collission_count++;
            if (_verbose)
                fprintf(stdout, "test colliding %ld to %ld\n", hash, new_hash);
            hash = new_hash;
        }
    }

    // Add the SQL for the hash columns.
    std::string hash_buff = std::to_string(hash);
    return hash_buff;
}

/* ================================================================ */

/// Drain the pending store queue.
/// Caution: this is slightly racy; a writer could still be busy
/// even though this returns. (There's a window in writeLoop, between
/// the dequeue, and the busy_writer increment. I guess we should fix
/// this...
void PGAtomStorage::flushStoreQueue()
{
    _write_queue.flush_queue();
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
void PGAtomStorage::storeAtom(const AtomPtr& atom, bool synchronous)
{
    get_ids();

    // If a synchronous store, avoid the queues entirely.
    if (synchronous)
    {
        Database database(this);
        do_store_atom_recursive(database, atom);
        return;
    }
    _write_queue.enqueue(atom);
}

/**
 * Store a single atom and its outgoing sets recursively and synchronously,
 * which means the actual store is done in the calling thread before this
 * function returns.
 *
 * Returns the height of the atom.
 */
int PGAtomStorage::do_store_atom_recursive(Database& database, AtomPtr atom)
{
    int height = 0;

    // If this is a link...
    if (atom->is_link())
    {
        // Handle the link case...
        OC_ASSERT(atom->is_link(), "atom Not Link or Node ???");

        // Loop over the outgoing set storing each one and returning the
        // height so we can know this atom's height.
        int max_out_atom_height = 0;
        for (auto& out_atom : atom->getOutgoingSet())
        {
            // Store the outgoing set atom and get its height.
            int atom_height = do_store_atom_recursive(database, out_atom);

            // Check the height against our current max.
            if (max_out_atom_height < atom_height)
                max_out_atom_height = atom_height;
        }

        // Height of this link is, by definition, one more than tallest
        // atom in outgoing set.
        height = max_out_atom_height + 1;
    }

    // Store the atom.
    do_store_atom_single(database, atom, height);

    // Return the height of this atom so callers can know their height 
    // if this was called recursively.
    return height;
}

void PGAtomStorage::vdo_store_atom(const AtomPtr& atom)
{
    Database database(this);
    do_store_atom_recursive(database, atom);
}

void PGAtomStorage::add_truth_value_columns(Database& database,
                                            AtomPtr atom)
{
    // Store the truth value into the columns

    // Store the truth value type...
    TruthValuePtr truth_ptr(atom->getTruthValue());
    TruthValueType truth_type = NULL_TRUTH_VALUE;
    if (truth_ptr)
        truth_type = truth_ptr->get_type();
    database.add_column_unsigned("tv_type", truth_type);

    // Store the mean, confidence and count according to the type.
    switch (truth_type)
    {
        case NULL_TRUTH_VALUE:
            break;
        case SIMPLE_TRUTH_VALUE:
        case COUNT_TRUTH_VALUE:
        case PROBABILISTIC_TRUTH_VALUE:
            database.add_column_double("stv_mean", truth_ptr->get_mean());
            database.add_column_double("stv_count", truth_ptr->get_count());
            database.add_column_double("stv_confidence", 
                    truth_ptr->get_confidence());
            break;
        case INDEFINITE_TRUTH_VALUE:
        {
            IndefiniteTruthValuePtr intentional_ptr = 
                    std::static_pointer_cast<const IndefiniteTruthValue>(
                    truth_ptr);
            database.add_column_double("stv_mean", intentional_ptr->getL());
            database.add_column_double("stv_count", intentional_ptr->getU());
            database.add_column_double("stv_confidence",
                    intentional_ptr->getConfidenceLevel());
            break;
        }
        default:
            throw RuntimeException(TRACE_INFO,
                "Error: store_single: Unknown truth value type\n");
    }
}

std::string PGAtomStorage::build_atom_insert(Database& database,
                                             AtomPtr atom,
                                             int height,
                                             int out_differentiator)
{
    // Begin the insert...
    database.start_insert("Atoms");

    // Store the atom's UUID.
    UUID uuid = TLB::addAtom(atom, TLB::INVALID_UUID);
    database.add_column_bigint("uuid", uuid);

    // Store the atomspace UUID. Since we allow storage of atoms that
    // don't belong to an atomspace, we need to handle that here by
    // adding a 0 space. The spaces table has a zero space entry so 
    // the reference constraint isn't violated by these inserts.
    AtomTable * atom_table = getAtomTable(atom);
    if (atom_table)
        database.add_column_bigint("space", atom_table->get_uuid());
    else
        database.add_column_bigint("space", 0);

    // Store the atom type mapped to the database type.
    Type atom_type = atom->get_type();
    int database_type = _storing_type_map[atom_type];
    database.add_column_unsigned("type", database_type);

    // Store the node name, if its a node
    if (atom->is_node())
    {
        // The Atoms table has a UNIQUE constraint on the
        // node name.  If a node name is too long, a postgres
        // error is generated:
        // ERROR: index row size 4440 exceeds maximum 2712
        // for index "atoms_type_name_key"
        // There's not much that can be done about this, without
        // a redesign of the table format, in some way. Maybe
        // we could hash the long node names, store the hash,
        // and make sure that is unique.
        database.add_column_quoted_string("name", atom->get_name(), 
                MAX_NODE_NAME_LENGTH);

        // Nodes have a height of zero by definition.
        database.add_column_unsigned("height", 0);
    }
    else
    {
        // Handle the Link case
        OC_ASSERT(atom->is_link(), "atom Not Link or Node ???");

        // See if this height is a new max.
        if (max_height < height)
            max_height = height;

        // Setup the height column
        database.add_column_unsigned("height", height);

        // If this is a link and we're not storing edges separately.
        int arity = atom->get_arity();
        if (_store_edges)
        {
            if (arity)
            {
                // Hash the outgoing set.
                const HandleSeq& outgoing = atom->getOutgoingSet();
                std::string hash = outgoing_set_to_hash_string(outgoing);

                // Add the SQL for the hash columns.
                database.add_column_string("out_hash", hash);
                database.add_column_unsigned("out_differentiator",
                        out_differentiator);
            }
        }
        else
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
            if (arity)
            {
                if (arity > 330)
                {
                    throw RuntimeException(TRACE_INFO, "Error: "
                        "do_store_atom_single: Maxiumum Link size is 330.\n");
                }

                const HandleSeq& outgoing_set = atom->getOutgoingSet();
                std::string outgoing = outgoing_set_to_string(outgoing_set);
                database.add_column_string("outgoing", outgoing.c_str());
            }
        }
    } 

    // // Add the truth value columns to the update.
    add_truth_value_columns(database, atom);

    // Build the statement and return it.
    std::string statement = database.build_insert_statement();
    return statement;
}

std::string PGAtomStorage::build_atom_update(Database& database,
                                             AtomPtr atom)
{
    // Start the update statement.
    database.start_update("Atoms");

    // Add the truth value columns to the update.
    add_truth_value_columns(database, atom);

    // Build the statement and return it.
    UUID uuid = TLB::addAtom(atom, TLB::INVALID_UUID);
    std::string statement = database.build_update_where("uuid", uuid);
    return statement;
}


void PGAtomStorage::do_store_atom_single(Database& database, 
                                         AtomPtr atom,
                                         int height)
{
    // Use the TLB Handle as the UUID.
    Handle h(atom->get_handle());
    if (isInvalidHandle(h))
    {
        throw RuntimeException(TRACE_INFO, 
                "Trying to save atom with an invalid handle!");
    }

    // Check the lock to see if this atom has been inserted or not.
    UUID uuid = TLB::addAtom(h, TLB::INVALID_UUID);
    std::unique_lock<std::mutex> lck = maybe_create_id(uuid);
    bool atom_needs_insert = lck.owns_lock();

    // Store the atom type and node name only if storing for the
    // first time ever. Once an atom is in an atom table, it's
    // name can type cannot be changed. Only its truth value can
    // change.
    std::string statement;
    if (atom_needs_insert)
        statement = build_atom_insert(database, atom, height);
    else
        statement = build_atom_update(database, atom);
    
    // Execute the statement.
    database.execute(statement.c_str());

    // If there was an error on an insert...
    if (atom_needs_insert and not database.has_results())
    {
        // If we get there there could be three separate reasons:
        //
        // 1) The query failed due to a database or query problem.
        // 2) The spaces table reference constraint failed.
        // 3) The link out_hash collided with another atom.
        //
        // Not much we can do about 1), it should already get reported
        // to stderr. But we can recover from 2) and 3) so we'll attempt
        // to do so here.

        // We may have to store the atom table UUID and try again...
        // We waste CPU cycles to store the atomtable, only if it failed.
        AtomTable *at = getAtomTable(atom);
        if (at) store_atomtable_id(*at);

        // Now retry the statement.
        database.execute(statement.c_str());

        // If there was still an error, handle the possibility that it
        // was caused by a collision of the hash on the outgoing set if
        // this was a link.
        if (not database.has_results() and _store_edges and atom->is_link())
        {
            // Get a new diffentiator for this atom's uuid.
            int differentiator = load_max_hash_differentiator(atom->get_type(), 
                    atom->getOutgoingSet());

            // Try again with an incremented differentiator. We'll return
            // from here since the atom will have been added after this call.
            differentiator++;

            // Now try storing again with this new differentiator.
            statement = build_atom_insert(database, atom, height, 
                    differentiator);
            database.execute(statement.c_str());
        }
    }

    // Store the outgoing handles.
    if (atom_needs_insert)
    {
        // If this is a link then store it's edges...
        if (_store_edges and atom->is_link())
            store_outgoing_edges(atom);
    }

    // Make note of the fact that this atom has been stored.
    add_id_to_cache(uuid);
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
 * Given an opencog type t, the _storing_type_map[t] will contain the
 * sqlid for the named type. The _storing_type_map[t] will *always*
 * contain a valid value.
 *
 * Given an SQL type sq, the _loading_type_map[sq] will contain the
 * opencog type t for the named type, or NOTYPE if this version of
 * opencog does not have this kind of atom.
 *
 * The type_maps must be constructed before any saving or loading of
 * atoms can happen. The type_maps will be a superset (union) of the
 * types used by OpenCog, and stored in the SQL table.
 */
void PGAtomStorage::setup_type_map(void)
{
    Database database(this);

    /* Only need to set up the type_map once. */
    if (type_map_was_loaded) return;
    type_map_was_loaded = true;

    // If we are here, we need to reconcile the types currently in
    // use, with a possibly pre-existing type_map. New types must be
    // stored.  So we start by loading a map from SQL (if its there).
    //
    // Be careful to initialize the type_map with invalid types,
    // in case there are unexpected holes in the map!
    for (int i=0; i< TYPEMAP_SZ; i++)
    {
        _loading_type_map[i] = NOTYPE;
        _storing_type_map[i] = -1;
        _database_type_names[i] = NULL;
    }

    // Load the database types from the TypeCodes table.
    database.execute("SELECT * FROM TypeCodes;");
    database.for_each_row(&Database::type_cb);

    // Now loop over each type that the nameserver knows about and
    // map it to the database types.
    unsigned int numberOfTypes = nameserver().getNumberOfClasses();
    for (Type t=0; t<numberOfTypes; t++)
    {
        int type_codes_type = _storing_type_map[t];
        /* If this typename is not yet known, record it */
        if (-1 == type_codes_type)
        {
            const char * type_name = nameserver().getTypeName(t).c_str();

            // Let the sql id be the same as the current type number,
            // unless this sql number is already in use, in which case
            // we need to find another, unused one.  Its in use if we
            // have a string name associated to it.
            type_codes_type = t;

            if ((_database_type_names[type_codes_type] != NULL) &&
                (_loading_type_map[type_codes_type] != t))
            {
                // Find some (any) unused type index to use in the
                // sql table. Use the lowest unused value that we
                // can find.
                for (type_codes_type = 0; type_codes_type < TYPEMAP_SZ;
                     type_codes_type++)
                {
                    if (NULL == _database_type_names[type_codes_type])
                        break;
                }

                if (TYPEMAP_SZ <= type_codes_type)
                {
                    fprintf(stderr, "Fatal Error: type table overflow!\n");
                    abort();
                }
            }

            char statement[BUFFER_SIZE];
            snprintf(statement, BUFFER_SIZE,
                     "INSERT INTO TypeCodes (type, typename) "
                     "VALUES (%d, \'%s\');",
                     type_codes_type, type_name);
            database.execute(statement);
            map_database_type(type_codes_type, type_name);
        }
    }
}

void PGAtomStorage::map_database_type(int dbval, const char * type_name)
{
    Type realtype = nameserver().getType(type_name);
    _loading_type_map[dbval] = realtype;
    _storing_type_map[realtype] = dbval;
    if (_database_type_names[dbval] != NULL) free (_database_type_names[dbval]);
    _database_type_names[dbval] = strdup(type_name);
}

/* ================================================================ */
/**
 * Add a single UUID to the ID cache. Thread-safe.
 * This also unlocks the id-creation lock, if it was being held.
 */
void PGAtomStorage::add_id_to_cache(UUID uuid)
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
 */
std::unique_lock<std::mutex> PGAtomStorage::maybe_create_id(UUID uuid)
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
void PGAtomStorage::get_ids(void)
{
    Database database(this);
    std::unique_lock<std::mutex> lock(id_cache_mutex);

    if (local_id_cache_is_inited) return;
    local_id_cache_is_inited = true;

    local_id_cache.clear();

    // Load in chunks to reduce resource requirements and because some
    // ODBC drivers don't handle large result sets well.    
    unsigned long chunk_start;
    unsigned long max_uuid = load_max_atoms_uuid();
    for (chunk_start = 0; chunk_start <= max_uuid; chunk_start += LOAD_CHUNK)
    {
        char statement[BUFFER_SIZE];
        snprintf(statement, BUFFER_SIZE, "SELECT uuid FROM Atoms WHERE "
                "uuid > %lu AND uuid <= %lu;",
                 chunk_start, chunk_start + LOAD_CHUNK);

        database.id_set = &local_id_cache;
        database.execute(statement);
        database.for_each_row(&Database::note_id_cb);
    }
}

/* ================================================================ */

void PGAtomStorage::get_outgoing_edges(UUID uuid, std::vector<UUID>& outgoing)
{
    auto search = _edge_cache.find(uuid);
    if(search != _edge_cache.end())
    {
        // fprintf(stdout, "get_outgoing_edges - found %lu\n", uuid);
        outgoing = search->second;
    }
    else
    {
        Database database(this);

        // fprintf(stdout, "get_outgoing_edges - not cached %lu\n", uuid);

        // Execute the select database.
        char statement[BUFFER_SIZE];
        snprintf(statement, BUFFER_SIZE, "SELECT dst_uuid FROM Edges "
                "WHERE src_uuid = %lu ORDER by pos;", uuid);
        database.execute(statement);

        // Reserve our initial chunk for the outgoing vector.
        int growth_chunk = INITIAL_GROWTH_CHUNK;
        int out_size = growth_chunk;
        outgoing.reserve(out_size);

        // Preparing to optimize for multi-atom selects, and since we only 
        // need one column we'll use an optimized retrieval of the column
        // to avoid string lookups each time.

        int row_count = 0;
        while (database.fetch_next_row())
        {
            // Grow the outgoing vector if needed. Keep increasing the growth
            // chunk so we keep memory allocation and copies down for larger
            // outgoing sets.
            if (row_count >= out_size)
            {
                out_size += growth_chunk;
                growth_chunk <<= 1;
                outgoing.reserve(out_size);
            }

            // Place the UUID it directly into the outgoing set.
            const char* uuid_chars = database.column_value(0);
            outgoing.emplace_back(strtoul(uuid_chars, (char **) NULL, 10));
            row_count++;
        }

        // Cache the outgoing edges for this uuid since we may need it right
        // away if we're looking for collisions.

        // Make room if the cache is full.
        if (_edge_cache.size() >= EDGE_CACHE_MAX_SIZE)
            _edge_cache.erase(_edge_cache.begin());

        // Add this outgoing to the cache.
        _edge_cache.emplace(uuid, outgoing);
    }
}

int PGAtomStorage::load_max_hash_differentiator(Type t, 
                                                const HandleSeq& outgoing)
{
    Database database(this);
    int db_type = _storing_type_map[t];
    char statement[BUFFER_SIZE];
    std::string hash = outgoing_set_to_hash_string(outgoing);
    snprintf(statement, BUFFER_SIZE, "SELECT MAX(out_differentiator) "
            "FROM Atoms WHERE "
            " type = %d AND out_hash = %s;", db_type, hash.c_str());
    database.execute(statement);
    return database.get_int_result();
}

/* ================================================================ */

/* One-size-fits-all atom fetcher */
PGAtomStorage::PseudoPtr PGAtomStorage::load_pseudo_atom(const char * query, 
                                                         int height)
{
    Database database(this);
    database.uuid = TLB::INVALID_UUID;
    database.execute(query);
    database.row_count = 0;
    database.for_each_row(&Database::create_atom_cb);

    // Did we actually find anything?
    // DO NOT USE IsInvalidHandle() HERE! It won't work, duhh!
    if (database.uuid == TLB::INVALID_UUID)
        return NULL;

    // Now check to make sure we don't have more than one

    database.height = height;
    PseudoPtr atom(make_pseudo_atom(database, database.uuid));
    return atom;
}

PGAtomStorage::PseudoPtr PGAtomStorage::load_pseudo_atom_with_uuid(UUID uuid)
{
    char statement[BUFFER_SIZE];
    snprintf(statement, BUFFER_SIZE,
            "SELECT * FROM Atoms WHERE uuid = %lu;",uuid);

    return load_pseudo_atom(statement, -1);
}

void PGAtomStorage::cache_atom(UUID uuid, AtomPtr atom)
{
    // Remove an atom if we've reached the cache size.
    if (_atom_cache.size() > ATOM_CACHE_SIZE)
        _atom_cache.erase(_atom_cache.begin());

    // Add the new atom to the cache.
    _atom_cache.emplace(uuid, atom);
}

AtomPtr PGAtomStorage::get_cached_atom(UUID uuid)
{
    // Remove an atom if we've reached the cache size.
     auto search = _atom_cache.find(uuid);
    if(search != _atom_cache.end())
    {
        // fprintf(stdout, "get_cached_atom - found %lu\n", uuid);
        return search->second;
    }

    // We didn't find the UUID..
    return NULL;
}


/**
 * Retreive the entire incoming set of the indicated atom.
 */
HandleSeq PGAtomStorage::getIncomingSet(const Handle& h)
{
    Database database(this);
    char statement[BUFFER_SIZE];
    HandleSeq incoming_set;

    UUID uuid = TLB::addAtom(h, TLB::INVALID_UUID);
    if (_store_edges)
    {
        snprintf(statement, BUFFER_SIZE,
                "SELECT * FROM Atoms WHERE uuid IN"
                "(SELECT src_uuid FROM Edges WHERE dst_uuid = %lu);",
                uuid);
    }
    else
    {
        snprintf(statement, BUFFER_SIZE,
            "SELECT * FROM Atoms WHERE outgoing @> ARRAY[CAST(%lu AS BIGINT)];",
            uuid);

        // Note: "select * from atoms where outgoing@>array[556];" will return
        // all links with atom 556 in the outgoing set -- i.e. the incoming set
        // of 556. Could also use && here instead of @> Don't know if one is
        // faster or not. The cast to BIGINT is needed, as otherwise one gets
        // ERROR:  operator does not exist: bigint[] @> integer[]
    }

    // Now execute the query.
    database.height = -1;
    database.hvec = &incoming_set;
    database.execute(statement);

    // Process the rows for the incoming set and return it.
    database.for_each_row(&Database::fetch_incoming_set_cb);
    return incoming_set;
}

/**
 * Fetch Node from database, with the indicated type and name.
 * If there is no such node, NULL is returned.
 * More properly speaking, the point of this routine is really
 * to fetch the associated TruthValue for this node.
 *
 * This method does *not* register the atom with any atomtable/atomspace
 */
TruthValuePtr PGAtomStorage::getNode(Type t, const char * str)
{
    char statement[40*BUFFER_SIZE];

    // Use postgres $-quoting to make unicode strings easier to deal with.
    int nc = snprintf(statement, 4*BUFFER_SIZE, "SELECT * FROM Atoms WHERE "
        "type = %hu AND name = $ocp$%s$ocp$ ;", _storing_type_map[t], str);

    if (40*BUFFER_SIZE-1 <= nc)
    {
        fprintf(stderr, "Error: PGAtomStorage::getNode: buffer overflow!\n");
        statement[40*BUFFER_SIZE-1] = 0x0;
        fprintf(stderr, "\tnc=%d buffer=>>%s<<\n", nc, statement);
        return Handle();
    }

    PseudoPtr p = load_pseudo_atom(statement, 0);
    if (nullptr == p) return TruthValuePtr();

    NodePtr node = createNode(t, str, p->tv);
    TLB::addAtom(node, p->uuid);
    return p->tv;
}

bool PGAtomStorage::outgoing_matches_uuids(const HandleSeq& outgoing,
                                           std::vector<UUID> uuids)
{
    // First check that the sizes match.
    if (outgoing.size() != uuids.size())
        return false;

    // Loop checking the handles against the uuids vector.
    int atom_index = 0;
    for (auto atom : outgoing)
    {
        // If the atom's UUID doesn't match the outgoing UUID at
        // the index they are not the same since order matters for
        // the outgoing sets.
        UUID uuid = TLB::addAtom(atom, TLB::INVALID_UUID);
        if (uuid != uuids[atom_index])
            return false;

        // Increment the index.
        atom_index++;
    }

    // If we get here they matched so return true.
    return true;    
}


/**
 * Fetch Link from database, with the indicated type and outgoing set.
 * If there is no such link, NULL is returned.
 * More properly speaking, the point of this routine is really
 * to fetch the associated TruthValue for this link.
 *
 * This method does *not* register the atom with any atomtable / atomspace
 */
TruthValuePtr PGAtomStorage::getLink(const Handle& h)
{
    Type type = h->get_type();
    const HandleSeq& outgoing = h->getOutgoingSet();
    Database database(this);
    database.uuid = TLB::INVALID_UUID;
    char statement[BUFFER_SIZE];

    // If we're storing edges...
    if (_store_edges)
    {
        // Print out the outgoing set for verbose debugging.
        if (_verbose)
        {
            std::string out_string = outgoing_set_to_string(outgoing);
            fprintf(stdout, "getLink - outgoing = %s\n", out_string.c_str());
        }

        // Hash the outgoing set.   
        std::string hash = outgoing_set_to_hash_string(outgoing);

        // Execute the select statement.
        snprintf(statement, BUFFER_SIZE,
                "SELECT * FROM Atoms WHERE type = %hu AND out_hash = %s;",
                _storing_type_map[type], hash.c_str());
        database.execute(statement);

        // Since we could have a hash collision, we can't assume we have a 
        // match until we check the outgoing set for equality against the
        // uuids retrieved from the Atoms table. We also can't assume there
        // is just one row that returns since there might be more than one
        // row with the same hash but separate out_differentiator values.
        bool set_matches = false;
        std::vector<UUID> row_uuids;
        while (not set_matches and database.fetch_next_row())
        {
            // Get the columns loaded using the normal callback.
            database.for_each_column(&Database::create_atom_column_cb);

            // Get the edges from the Edges table...
            get_outgoing_edges(database.uuid, row_uuids);

            // See if we get a match. If so, we are done.
            set_matches = outgoing_matches_uuids(outgoing, row_uuids);
        }
    }
    else
    {
        std::string out_string = outgoing_set_to_string(outgoing);
        snprintf(statement, BUFFER_SIZE,
            "SELECT * FROM Atoms WHERE type = %hu AND outgoing = %s;",
            _storing_type_map[type], out_string.c_str());
        database.execute(statement);
        database.row_count = 0;
        database.for_each_row(&Database::create_atom_cb);
    }

    // Did we actually find anything? DO NOT USE IsInvalidHandle() HERE! 
    // It won't work, duhh!
    if (database.uuid == TLB::INVALID_UUID)
        return TruthValuePtr();

    // If we get here, we have the real Atoms column data loaded and the
    // collisions have been handled so the outgoing set matches the
    // uuids returned by the database query.
    database.height = 1;
    PseudoPtr pseudo_atom(make_pseudo_atom(database, database.uuid));

    // Create the actual link.
    LinkPtr link = createLink(type, outgoing, pseudo_atom->tv);
    TLB::addAtom(link, pseudo_atom->uuid);
    return pseudo_atom->tv;
}

/**
 * Instantiate a new atom, from the response buffer contents
 */
PGAtomStorage::PseudoPtr PGAtomStorage::make_pseudo_atom(Database &database,
                                                         UUID uuid)
{
    // Now that we know everything about an atom, actually construct one.
    Type realtype = _loading_type_map[database.itype];

    if (NOTYPE == realtype)
    {
        throw RuntimeException(TRACE_INFO,
            "Fatal Error: OpenCog does not have a type called %s\n",
            _database_type_names[database.itype]);
        return NULL;
    }

    // Make a shared pseudo-atom pointer for this new atom.
    PseudoPtr pseudo_atom = std::make_shared<PseudoAtom>();

    // All height zero atoms are nodes,
    // All positive height atoms are links.
    // A negative height is "unknown" and must be checked.
    if ((0 == database.height) or
        ((-1 == database.height) and nameserver().isA(realtype, NODE)))
    {
        // Handle the Node case
        pseudo_atom->name = database.name;

        if (_verbose)
        {
            fprintf(stdout, "  %6lu %s, %s\n", uuid,
                    nameserver().getTypeName(realtype).c_str(), database.name);
        }
    }
    else
    {
        // Handle the link case

        // If we are storing edges in the Edges table..
        if (_store_edges)
        {
            // Get the edges from the Edges table...
            get_outgoing_edges(uuid, pseudo_atom->oset);
        }

        // Otherwise this will already have been loaded into the 
        // response as part of the 'outgoing' column.
        else
        {
            char *p = (char *) database.outlist;
            while (p)
            {
                // Break if there are no more atoms in the outgoing set
                // or if the outgoing set is empty in the first place.
                if (*p == '}' or *p == '\0') break;
                UUID out(strtoul(p+1, &p, 10));
                pseudo_atom->oset.emplace_back(out);
            }
        }

        if (_verbose)
        {
            fprintf(stdout, "  %6lu %s, arity %lu, { ", uuid,
                    nameserver().getTypeName(realtype).c_str(),
                    pseudo_atom->oset.size());
            for (auto uuid : pseudo_atom->oset)
                fprintf(stdout,"%lu ", uuid);
            fprintf(stdout, "}\n");
        }
    }

    // Give the atom the correct UUID. The AtomTable will need this.
    pseudo_atom->type = realtype;
    pseudo_atom->uuid = uuid;

    // Now get the truth value
    switch (database.tv_type)
    {
        case NULL_TRUTH_VALUE:
            break;

        case SIMPLE_TRUTH_VALUE:
        {
            TruthValuePtr stv(SimpleTruthValue::createTV(database.mean,
                    database.confidence));
            pseudo_atom->tv = stv;
            break;
        }
        case COUNT_TRUTH_VALUE:
        {
            TruthValuePtr ctv(CountTruthValue::createTV(database.mean,
                    database.confidence, database.count));
            pseudo_atom->tv = ctv;
            break;
        }
        case INDEFINITE_TRUTH_VALUE:
        {
            TruthValuePtr itv(IndefiniteTruthValue::createTV(database.mean,
                    database.count, database.confidence));
            pseudo_atom->tv = itv;
            break;
        }
        case PROBABILISTIC_TRUTH_VALUE:
        {
            TruthValuePtr ptv(ProbabilisticTruthValue::createTV(database.mean,
                    database.confidence, database.count));
            pseudo_atom->tv = ptv;
            break;
        }
        default:
            throw RuntimeException(TRACE_INFO,
                "Error: make_pseudo_atom: Unknown truth value type\n");
    }

    load_count ++;
    if (load_count % REPORTING_INTERVAL == 0)
    {
        fprintf(stderr, "  Loaded %10lu atoms.\n", (unsigned long) load_count);
    }

    add_id_to_cache(uuid);
    return pseudo_atom;
}

void PGAtomStorage::cache_edges_where(const char * where_clause)
{
    Database database(this);
    int growth_chunk = INITIAL_GROWTH_CHUNK;
    int out_size = growth_chunk;
    int row_count = 0;
    std::vector<UUID> outgoing;

    // Clear the edge cache. Just in case.
    _edge_cache.clear();

    // Execute the select database.
    char statement[BUFFER_SIZE];
    snprintf(statement, BUFFER_SIZE, "SELECT src_uuid, dst_uuid FROM Edges "
            "WHERE src_uuid IN (SELECT uuid FROM Atoms WHERE %s) "
            "ORDER by src_uuid, pos;", where_clause);
    database.execute(statement);

    // Since we know we have three columns, we'll use an optimized retrieval 
    // of the columns to avoid string lookups each time.

    // Loop over the rows building outgoing sets and caching each source
    // uuid's outgoing set separately.
    UUID last_source_uuid = NO_UUID;
    while (database.fetch_next_row())
    {
        // Get the source and destination uuids, and pos for this row
        const char* uuid_chars = database.column_value(0);
        UUID source_uuid = strtoul(uuid_chars, (char **) NULL, 10);
        uuid_chars = database.column_value(1);
        UUID destination_uuid = strtoul(uuid_chars, (char **) NULL, 10);

        // If this is a new uuid.
        if (source_uuid != last_source_uuid)
        {
            // Clear the outgoing vector of UUIDs.
            if (last_source_uuid != NO_UUID)
            {
                // Add the last uuid and outgoing vector to the cache.
                _edge_cache.emplace(last_source_uuid, outgoing);

                // Clear the outgoing for the next uuid.
                outgoing.clear();
            }

            // Reserve our initial chunk for the outgoing vector.
            growth_chunk = INITIAL_GROWTH_CHUNK;
            out_size = growth_chunk;
            outgoing.reserve(out_size);

            // Remember this new source uuid.
            last_source_uuid = source_uuid;
        }

        // Grow the outgoing vector if needed. Keep increasing the growth
        // chunk so we keep memory allocation and copies down for larger
        // outgoing sets.
        if (row_count >= out_size)
        {
            out_size += growth_chunk;
            growth_chunk <<= 1;
            outgoing.reserve(out_size);
        }

        // Place the destination UUID directly into the outgoing set.
        outgoing.emplace_back(destination_uuid);
        row_count++;
    }

    // Cache the last outgoing vector.
    if (last_source_uuid != NO_UUID)
    {
        // Add the last uuid and outgoing vector to the cache.
        _edge_cache.emplace(last_source_uuid, outgoing);
    }
}

/* ================================================================ */

void PGAtomStorage::load(AtomTable &atom_table)
{
    Database database(this, &atom_table);

    // Reserve the UUID range.
    UUID max_uuid = reserve_max_atoms_uuid();

    // Get the maximum height so we can run optimized queries using
    // height so that all links will already have their outgoing sets
    // available because we can load bottom up.
    max_height = load_max_atoms_height();
    fprintf(stderr, "  Max Height is %d\n", max_height);

    // Load the trees in bottom up order:
    //
    // height 0 - nodes
    // height 1 - links of nodes
    // height 2 - links of (nodes and (links of nodes))
    // etc...
    //
    load_count = 0;
    for (int height=0; height <= max_height; height++)
    {
        unsigned long count_start = load_count;
        bool cache_edges = height > 0 and _store_edges;

        // Load in chunks to reduce resource requirements and because some
        // ODBC drivers don't handle large result sets well.
        unsigned long chunk_start;
        for (chunk_start = 0; chunk_start <= max_uuid; chunk_start+= LOAD_CHUNK)
        {
            // Compute the WHERE clause separately since we'll be using that
            // for both the Atoms select and optionally the Edges cache.
            char where_clause[BUFFER_SIZE];
            snprintf(where_clause, BUFFER_SIZE, "height = %d AND "
                    "uuid > %lu AND uuid <= %lu",
                     height, chunk_start, chunk_start + LOAD_CHUNK);

            // If we are storing to the Edges table then read all the edges
            // for the atoms for this chunk in one statement.
            if (height > 0 and cache_edges)
                cache_edges_where(where_clause);

            // Now get the atoms. When they load, their outgoing set should 
            // already be cached.
            char statement[BUFFER_SIZE];
            snprintf(statement, BUFFER_SIZE, "SELECT * FROM Atoms WHERE %s "
                    "ORDER BY uuid;", where_clause);
            database.height = height;
            database.execute(statement);
            database.for_each_row(&Database::load_all_atoms_cb);

            // Purge the edge cache.
            if (cache_edges)
                _edge_cache.clear();
        }
        fprintf(stderr, "  Loaded %lu atoms at height %d\n", 
                load_count - count_start, height);
    }

    fprintf(stderr, "  Finished loading %lu atoms in total\n",
        (unsigned long) load_count);

    // Synchronize!
    atom_table.barrier();
}

void PGAtomStorage::loadType(AtomTable &atom_table, Type atom_type)
{
    Database database(this, &atom_table);

    // Reserve the UUIDs up to the max in the atoms table.
    unsigned long max_uuid = reserve_max_atoms_uuid();
    logger().debug("PGAtomStorage::loadType: Max observed UUID is %lu\n",
            max_uuid);
    load_count = 0;

    // For links, assume a worst-case height.
    // For nodes, its easy ... max_height is zero.
    if (nameserver().isNode(atom_type))
        max_height = 0;
    else
        max_height = load_max_atoms_height();
    logger().debug("PGAtomStorage::loadType: Max Height is %d\n", max_height);
    int db_atom_type = _storing_type_map[atom_type];

    for (int height=0; height <= max_height; height++)
    {
        unsigned long cur = load_count;

#if GET_ONE_BIG_BLOB
        char statement[BUFFER_SIZE];
        snprintf(statement, BUFFER_SIZE,
            "SELECT * FROM Atoms WHERE height = %d AND type = %d;",
             height, db_atom_type);
        database.height = height;
        database.execute(statement);
        database.for_each_row(&Database::load_if_not_exists_cb);
#else
        // It appears that, when the select statment returns more than
        // about a 100K to a million atoms or so, some sort of heap
        // corruption occurs in the iodbc code, causing future mallocs
        // to fail. So limit the number of records processed in one go.
        // It also appears that asking for lots of records increases
        // the memory fragmentation (and/or there's a memory leak in iodbc??)
        // XXX Not clear is UnixODBC suffers from this same problem.
#define STEP 12003
        unsigned long rec;
        for (rec = 0; rec <= max_uuid; rec += STEP)
        {
            char statement[BUFFER_SIZE];
            snprintf(statement, BUFFER_SIZE, "SELECT * FROM Atoms WHERE "
                    "type = %d AND height = %d AND uuid > %lu AND uuid <= %lu;",
                     db_atom_type, height, rec, rec+STEP);
            database.height = height;
            database.execute(statement);
            database.for_each_row(&Database::load_if_not_exists_cb);
        }
#endif
        logger().debug("PGAtomStorage::loadType: Loaded %lu atoms of type %d "
                "at height %d\n", load_count - cur, db_atom_type, height);
    }
    logger().debug("PGAtomStorage::loadType: Finished loading %lu atoms"
            " in total\n", (unsigned long) load_count);

    // Synchronize!
    atom_table.barrier();
}

void PGAtomStorage::store(const AtomTable &table)
{
    Database database(this);

    max_height = 0;
    store_count = 0;

#ifdef ALTER
    rename_tables();
    create_tables();
#endif

    store_atomtable_id(table);

    // Get the local cache of stored atoms.
    get_ids();

    UUID max_uuid = getMaxUUID();
    fprintf(stderr, "  Max UUID is %lu\n", max_uuid);

    // Drop indexes, for faster loading. This only matters for
    // the non-array edge storage.
    database.execute("DROP INDEX IF EXISTS src_idx;");
    database.execute("DROP INDEX IF EXISTS dst_idx;");

    // Loop over each atom...
    int per_transaction_count = 0;
    for (TypeIndex::iterator atom_iter = table.beginType(ATOM, true);
                             atom_iter != table.endType(); ++atom_iter)
    {
        // Get a reference for easier use.
        const AtomPtr& atom = *atom_iter;

        // Begin a transaction if we're using them.
        if (_transaction_chunk and per_transaction_count == 0)
            database.execute("BEGIN;");
    
        // Store the atom.
        int height = get_height(atom);
        do_store_atom_single(database, atom, height);

        // Increment out store counter and report.
        store_count ++;
        if (store_count % REPORTING_INTERVAL == 0)
        {
            fprintf(stderr, "  Stored %10lu atoms.\n",
                    (unsigned long) store_count);
        }

        // If we are using transactions for the stores...
        if (_transaction_chunk)
        {
            // Increment the per-transaction store count.
            per_transaction_count++;

            // If we've reached the transaction chunk do a commit.
            if (per_transaction_count >= _transaction_chunk)
            {
                database.execute("COMMIT;");
                per_transaction_count = 0;
            }
        }
    }

    // Create indexes
    database.execute("CREATE INDEX src_idx ON Edges (src_uuid, pos);");
    database.execute("CREATE INDEX dst_idx ON Edges (dst_uuid);");

    // Commit the last transaction chunk if there is one.
    if (_transaction_chunk and per_transaction_count > 0)
        database.execute("COMMIT;");

    // Cleanup PostgreSQL to free up any unused storage.
    database.execute("VACUUM ANALYZE;");

    // Set the new max height global.
    store_max_height_global(load_max_atoms_height());

    // We're done.
    fprintf(stderr, "  Finished storing %10lu atoms total.\n",
        (unsigned long) store_count);
}

/* ================================================================ */

void PGAtomStorage::rename_tables(void)
{
    Database database(this);

    database.execute("ALTER TABLE Spaces RENAME TO Spaces_Backup;");
    database.execute("ALTER TABLE Atoms RENAME TO Atoms_Backup;");
    database.execute("ALTER TABLE Edges RENAME TO Edges_Backup;");
    database.execute("ALTER TABLE TypeCodes RENAME TO TypeCodes_Backup;");
    database.execute("ALTER TABLE Global RENAME TO Global_Backup;");
}

void PGAtomStorage::create_tables(void)
{
    Database database(this);

    // See the file "atom.sql" for detailed documentation as to the
    // structure of the SQL tables.
    database.execute("CREATE TABLE Spaces ("
            "space     BIGINT PRIMARY KEY,"
            "parent    BIGINT);");

    database.execute("INSERT INTO Spaces VALUES (0,0);");
    database.execute("INSERT INTO Spaces VALUES (1,1);");

    // Add the default spaces to the cache.
    table_id_cache.clear();
    table_id_cache.insert((UUID) 0);
    table_id_cache.insert((UUID) 1);

    database.execute("CREATE TABLE Atoms ("
            "uuid                BIGINT PRIMARY KEY,"
            "space               BIGINT REFERENCES spaces(space),"
            "type                SMALLINT,"
            "type_tv             SMALLINT,"
            "stv_mean            FLOAT,"
            "stv_confidence      FLOAT,"
            "stv_count           DOUBLE PRECISION,"
            "height              SMALLINT,"
            "name                TEXT,"
            "out_hash            BIGINT,"
            "out_differentiator  SMALLINT,"
            "outgoing            BIGINT[],"
            "UNIQUE (type, name),"
            "UNIQUE (type, out_hash, out_differentiator),"
            "UNIQUE (type, outgoing));");

    database.execute("CREATE TABLE Edges ("
            "src_uuid  INT,"
            "dst_uuid  INT,"
            "pos INT);");

    database.execute("CREATE INDEX src_idx ON Edges (src_uuid, pos);");
    database.execute("CREATE INDEX dst_idx ON Edges (dst_uuid);");

    database.execute("CREATE TABLE TypeCodes ("
            "type SMALLINT UNIQUE,"
            "typename TEXT UNIQUE);");
    type_map_was_loaded = false;

    database.execute("CREATE TABLE Global ("
            "max_height INT);");
    database.execute("INSERT INTO Global (max_height) VALUES (0);");
}

/**
 * kill_data -- destroy data in the database!! Dangerous !!
 * This routine is meant to be used only for running test cases.
 * It is extremely dangerous, as it can lead to total data loss.
 */
void PGAtomStorage::kill_data(void)
{
    Database database(this);

    // See the above create_tables() for details on the structure.

    // Truncate all the tables.
    database.execute("TRUNCATE Atoms, Edges, Spaces, TypeCodes "
            "RESTART IDENTITY;");

    // Add back the default spaces.
    database.execute("INSERT INTO Spaces VALUES (0,0);");
    database.execute("INSERT INTO Spaces VALUES (1,1);");

    // Clear the table ID cache. We already added the default spaces 0 and 1.
    table_id_cache.clear();
    table_id_cache.insert((UUID) 0);
    table_id_cache.insert((UUID) 1);

    // Reset the max height global.
    database.execute("UPDATE Global SET max_height = 0;");

    // Report and reset the collision count.
    if (_generate_collisions)
        fprintf(stderr, "  Generated %d test collisions.\n", _collission_count);
    _collission_count = 0;

    // Reset the query count.
    _query_count = 0;
}

/* ================================================================ */

void PGAtomStorage::store_max_height_global(int height)
{
    Database database(this);

    // Remember this as the new max if it is higher.
    if (height > max_height)
        max_height = height;

    // Always set the height in the database so we know it matches.
    char statement[BUFFER_SIZE];
    snprintf(statement, BUFFER_SIZE, "UPDATE Global SET max_height = %d;",
            max_height);
    database.execute(statement);
}

int PGAtomStorage::load_max_height_global(void)
{
    Database database(this);
    database.execute("SELECT max_height FROM Global;");
    return database.get_int_result();
}

UUID PGAtomStorage::load_max_atoms_uuid(void)
{
    Database database(this);
    database.execute("SELECT uuid FROM Atoms ORDER BY uuid DESC LIMIT 1;");
    return database.get_int_result();
}

int PGAtomStorage::load_max_atoms_height(void)
{
    Database database(this);
    database.execute("SELECT height FROM Atoms ORDER BY height DESC LIMIT 1;");
    return database.get_int_result();
}

UUID PGAtomStorage::reserve_max_atoms_uuid(void)
{
    UUID max_uuid = load_max_atoms_uuid();
    fprintf(stderr, "  Reserving UUIDs up to %lu Atoms table max.\n", max_uuid);
    reserveMaxUUID(max_uuid);
    return max_uuid;
}

#endif /* HAVE_SQL_STORAGE */
/* ============================= END OF FILE ================= */
