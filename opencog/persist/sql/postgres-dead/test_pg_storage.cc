/*
 * FUNCTION:
 * Simple test to see if basic PostgreSQL atom storage is working.
 *
 * HISTORY:
 * Copyright (c) 2008 Linas Vepstas <linas@linas.org>
 *
 * Updated test by Curtis Faith for PostgreSQL storage - Feb 29 2016
 */

#ifdef HAVE_PGSQL_STORAGE

#include <time.h>
#include <ctime>
#include <chrono>
#include <cstring>

#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/truthvalue/SimpleTruthValue.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atomspaceutils/RandomAtomGenerator.h>

#include <opencog/persist/sql/SQLBackingStore.h>
#include <opencog/persist/sql/postgres/PGAtomStorage.h>

#include "libpq-fe.h"

using namespace opencog;

const int DONT_EXPAND_DBNAME = 0;


void print_time()
{
  time_t rawtime;
  struct tm * timeinfo;
  char buffer[80];

  time (&rawtime);
  timeinfo = localtime(&rawtime);

  strftime(buffer,80,"%d-%m-%Y %I:%M:%S",timeinfo);
  fprintf(stdout, "%s\n", buffer);
}


void exit_nicely(PGconn* connection)
{
    PQfinish(connection);
    exit(1);
}

/*
CREATE TABLE Atoms (
    -- The uuid maps to the atom handle.
    -- Must be unique, and must be non-null. Ergo, primary key.
    -- Use 64-bit int to match the C++ range.
    uuid    BIGINT PRIMARY KEY,

    -- The atomspace that this atom belongs to.
    space BIGINT REFERENCES spaces(space),

    -- Atom type, e.g. Link, Node, etc.
    type  SMALLINT,

    -- Inlined (simple) truth values
    tv_type  SMALLINT,
    stv_mean FLOAT,
    stv_confidence FLOAT,
    stv_count DOUBLE PRECISION,

    height SMALLINT,

    -- The node name, non-empty only for nodes
    name    TEXT,

    -- An array of the outgoing edges; non-empty only for links
    outgoing BIGINT[],

    -- Force the uniqueness of atoms!!
    UNIQUE (type, name),
    UNIQUE (type, outgoing)
*/

#define INSERT_COUNT 1000
#define LOOP_COUNT 1000
#define INSERT_BUFFER_SIZE 2048 
#define VALUES_BUFFER_SIZE 256  
#define STATEMENT_BUFFER_SIZE (INSERT_BUFFER_SIZE + \
                              (INSERT_COUNT * VALUES_BUFFER_SIZE))

#define MICROSECONDS_PER_SEC ((double) 1000000)

#define USE_TRANSACTIONS true 
#define DONT_USE_TRANSACTIONS false

#define GROUP_INSERTS true 
#define DONT_GROUP_INSERTS false

char empty_string[] = "";
char comma_string[] = ",";

using namespace std::chrono;
class ElapsedTimer
{
    steady_clock::time_point _begin_time;
    steady_clock::time_point _end_time;

    clock_t _begin_cpu_clock;
    clock_t _end_cpu_clock;

    std::string _description;
    size_t _op_count;
    std::string _op_type;

public:
    ElapsedTimer(std::string description,
                 size_t op_count = 0,
                 std::string op_type = "");
    ~ElapsedTimer();
};

ElapsedTimer::ElapsedTimer(std::string description,
             size_t op_count,
             std::string op_type)
{
    _description = description;
    _op_count = op_count;
    _op_type = op_type;

    fprintf(stdout, "\nStart: %-26s ", _description.c_str());
    print_time();
    _begin_time = steady_clock::now();
    _begin_cpu_clock = clock(); 
}

ElapsedTimer::~ElapsedTimer()
{
    double elapsed_time;
    double elapsed_per_op;
    double ops_per_second;
    std::string type_per_second = _op_type + "s / sec";

    _end_time = steady_clock::now();
    _end_cpu_clock = clock();   

    fprintf(stdout, "\nFinished: %-23s ", _description.c_str());
    print_time();

    elapsed_time = ((double) duration_cast<microseconds> 
            (_end_time - _begin_time).count()) / MICROSECONDS_PER_SEC;
    fprintf(stdout, "Elapsed time      %-15s = %12.2f seconds\n", "", 
            elapsed_time);
    if (_op_count)
    {
        elapsed_per_op = elapsed_time / _op_count * MICROSECONDS_PER_SEC;
        fprintf(stdout, "Elapsed time per  %-15s = %12.2f µs\n", 
                _op_type.c_str(), elapsed_per_op);
        ops_per_second = _op_count / elapsed_time;
        fprintf(stdout, "Elapsed %-15s           = %9.f\n", 
                type_per_second.c_str(), ops_per_second);
    }

    elapsed_time = ((double) _end_cpu_clock - (double) _begin_cpu_clock) /
            (double) CLOCKS_PER_SEC;
    fprintf(stdout, "CPU time      %-15s     = %12.2f seconds\n", "", 
            elapsed_time);
    if (_op_count)
    {
        elapsed_per_op = elapsed_time / _op_count * MICROSECONDS_PER_SEC;
        fprintf(stdout, "CPU time per %-15s      = %12.2f µs\n", 
                _op_type.c_str(), elapsed_per_op);
        ops_per_second = _op_count / elapsed_time;
        fprintf(stdout, "CPU %-15s               = %9.f\n", 
                type_per_second.c_str(), ops_per_second);
    }

    fprintf(stdout, "\n");
}

void insert_atoms_exec(PGconn* connection,
                       bool use_transactions,
                       bool group_inserts)
{

    PGresult *result;
    char insert_part[INSERT_BUFFER_SIZE];
    char values_part[VALUES_BUFFER_SIZE];
    char statement[STATEMENT_BUFFER_SIZE];
    size_t insert_length;
    size_t values_length;
    size_t statement_length;

    snprintf(statement, STATEMENT_BUFFER_SIZE, "DELETE FROM Atoms;");
    result = PQexec(connection, statement);
    if (!result || PQresultStatus(result) != PGRES_COMMAND_OK)
    {
        fprintf(stderr, "insert_atoms_exec - DELETE FROM Atoms failed\n");
        fprintf(stderr, "  error = %s\n", PQresultErrorMessage(result));
        PQclear(result);
    }

    if (use_transactions)
        fprintf(stdout,"\ninsert_atoms_exec with Transactions, ");
    else
        fprintf(stdout,"\ninsert_atoms_exec NO   Transactions, ");
    if (group_inserts)
        fprintf(stdout," insert grouping\n");
    else
        fprintf(stdout," NO     grouping\n");

    {   // Begin timed section...
        ElapsedTimer timer("atom inserts", LOOP_COUNT * INSERT_COUNT, "insert");

        // Generate the insert part.
        snprintf(insert_part, INSERT_BUFFER_SIZE, "INSERT INTO Atoms "
                "(uuid, space, type, "
                "tv_type, stv_mean, stv_confidence, stv_count, "
                "height, name)"
    //          "height, name, outgoing)"
                " VALUES ");
        insert_length = strlen(insert_part);

        for (int loop = 0; loop < LOOP_COUNT; loop++)
        {
            if (use_transactions)
            {
                snprintf(statement, STATEMENT_BUFFER_SIZE, "BEGIN;");
                result = PQexec(connection, statement);
                if (!result || PQresultStatus(result) != PGRES_COMMAND_OK)
                {
                    fprintf(stderr, 
                            "insert_atoms_exec - BEGIN failed\n");
                    fprintf(stderr, 
                            "  error = %s\n", PQresultErrorMessage(result));
                    PQclear(result);
                }
            }

            char* separator = empty_string;
            bool execute = false;
            for (int insert = 0; insert < INSERT_COUNT; insert++)
            {
                // Compute the uuid.
                unsigned long uuid = ((loop * INSERT_COUNT) + insert + 1);

                if (group_inserts)
                {
                    // If this is the first then copy the insert to the buffer.
                    if (insert == 0)
                    {
                        strcpy(statement, insert_part);
                        statement_length = insert_length;
                        separator = empty_string;
                        execute = false;
                    }
                    snprintf(values_part, VALUES_BUFFER_SIZE, "%s "
                            "(%lu, %lu, %u, "
                            "%u, %.8g, %.8g, %.8g, "
                            "%u, 'atom %lu')",
        //                  "%u, 'atom %lu','{}');",
                            separator, uuid, (unsigned long) 1, NODE, 
                            SIMPLE_TRUTH_VALUE, 0.0, 0.0, 0.0, 
                            0, uuid);
                    values_length = strlen(values_part);
                    strcpy(statement + statement_length, values_part);
                    statement_length += values_length;
                    separator = comma_string;

                    // If this in the last insert in the group...
                    if (insert == (INSERT_COUNT - 1))
                    {
                        // Terminate the statement.
                        strcpy(statement + statement_length, ";");

                        // Execute it this round.
                        execute = true;
                    }

                    // fprintf(stdout, "insert %d - %s\n", insert, statement);
                }
                else
                {
                    snprintf(statement, STATEMENT_BUFFER_SIZE, "%s "
                        "(%lu, %lu, %u, "
                        "%u, %.8g, %.8g, %.8g, "
                        "%u, 'atom %lu');",
        //              "%u, 'atom %u','{}');",
                        insert_part, uuid, (unsigned long) 1, NODE, 
                        SIMPLE_TRUTH_VALUE, 0.0, 0.0, 0.0, 
                        0, uuid);
                    execute = true;
                }

                // Execute it.
                if (execute)
                {
                    result = PQexec(connection, statement);
                    if (!result || PQresultStatus(result) != PGRES_COMMAND_OK)
                    {
                        fprintf(stderr, "insert_atoms_exec - INSERT failed\n");
                        fprintf(stderr, "  error = %s\n", 
                                    PQresultErrorMessage(result));
                        PQclear(result);
                    }
                }
            } // for insert

            if (use_transactions)
            {
                snprintf(statement, STATEMENT_BUFFER_SIZE, "COMMIT;");
                result = PQexec(connection, statement);
                if (!result || PQresultStatus(result) != PGRES_COMMAND_OK)
                {
                    fprintf(stderr, "insert_atoms_exec - COMMIT failed\n");
                    fprintf(stderr, "  error = %s\n", 
                                PQresultErrorMessage(result));
                    PQclear(result);
                }
            }
        } // for loop
    }
}

void generate_random_atoms(AtomSpace* atomspace, int total_atoms)
{
    unsigned long rand_seed = 10101010;
    float link_size_mean = 5.0f;
    RandomAtomGenerator random_atom_generator(atomspace, rand_seed,
        link_size_mean);
    ElapsedTimer timer("generate random atoms", total_atoms, "atom");

    int entry_atoms = atomspace->get_size();
    fprintf(stdout,"  atomspace contains %d atoms on entry\n", entry_atoms);

    // Generate half the random atoms.
    int total_random_atoms = 0;
    int random_atoms = total_atoms / 2;
    float percent_links = 0.3f;
    random_atom_generator.make_random_atoms(random_atoms, percent_links);
    total_random_atoms += random_atoms;

    // Generate another quarter with some two-level links.
    random_atoms = total_atoms / 4;
    random_atom_generator.make_random_atoms(random_atoms, percent_links);
    total_random_atoms += random_atoms;

    // Generate another quarter with some three-level links.
    random_atoms = total_atoms - total_random_atoms;
    random_atom_generator.make_random_atoms(random_atoms, percent_links);
    
    int exit_atoms = atomspace->get_size();
    fprintf(stdout,"  atomspace contains %d atoms on exit\n", exit_atoms);
    fprintf(stdout,"  should have generated %d atoms\n", total_atoms);
    fprintf(stdout,"  exit - entry = %d atoms created\n", exit_atoms - entry_atoms);
}

int main (int argc, char **argv)
{
    std::string db_database = "opencog_test";
    std::string db_user = "opencog_tester";
    std::string db_password = "cheese";
    int atom_count = 100;
    bool verbose = false;
    bool print_statements = false;
    bool store_edges = false;
    int transaction_chunk = 0;

    const char* usage_description = 
    "PostgreSQL Storage Tester\n\n"
    "Usage: test_pg_storage[options]\n"
    "  -h           \tdisplay this message showing options\n\n"
    "  -v           \tverbose output (default: false)\n\n"
    "  -q           \tprint SQL queries (default: false)\n\n"
    "  -e           \tstore edges (default: false)\n\n"
    "  -d <db>      \tdatabase name (default: opencog_test)\n\n"
    "  -u <user>    \tuser name (default: opencog_tester)\n\n"
    "  -p <pw>      \tpassword (default: cheese)\n\n"
    "  -s <int>     \ttest size (minimum: 20, default: 100)\n\n";

    // Don't report option errors except via '?' argument
    opterr = 0;
    int c;

    // Get each command line option...
    while ((c = getopt (argc, argv, "hvwqed:u:p:s:t:")) != -1) {
        switch (c)
        {
            case 'h':
                fprintf (stderr, "%s", usage_description);
                return 0;
            case 'v':
                verbose = true;
                break;
            case 'q':
                print_statements = true;
                break;
            case 'e':
                store_edges = true;
                break;
            case 'd':
                db_database = optarg;
                break;
            case 'u':
                db_user = optarg;
                break;
            case 'p':
                db_password = optarg;
                break;
            case 's':
                atom_count = atoi(optarg);
                if (atom_count < 20)
                {
                    fprintf(stdout, "test_pg_storage -s option of %d not used,"
                            " requires minumum 20 atoms.\n", atom_count);
                    atom_count = 20;
                }
                fprintf(stdout, "Generating %d random atoms.\n", atom_count);
                break;
            case 't':
                transaction_chunk = atoi(optarg);
                if (atom_count < 0)
                {
                    fprintf(stdout, "test_pg_storage -t option of %d was"
                            " negative.\n", transaction_chunk);
                    transaction_chunk = 0;
                    abort();
                }
                fprintf(stdout, "Using automatic transactions every %d "
                        "stores.\n", transaction_chunk);
                break;
            case '?':
                fprintf (stderr, "%s", usage_description);
                return 0;
            default:
                fprintf (stderr, "Unknown option %c ", optopt);
                abort ();
        }
    } // while there are options

    if (store_edges)
        std::cout << "Storing outgoing in Edges table" << std::endl;
    else
        std::cout << "Storing outgoing in Atoms outgoing {} array column" <<
                std::endl;

    static PGconn* connection;
    const char* keywords[] = {"dbname", "user", "password", NULL};
    const char* values[] = {db_database.c_str(), db_user.c_str(), 
            db_password.c_str(), NULL};

    // Open the database connection.
    connection = PQconnectdbParams(keywords, values, DONT_EXPAND_DBNAME);
    if (PQstatus(connection) != CONNECTION_OK)
    {
        fprintf(stderr, "Connection to database failed: %s",
                PQerrorMessage(connection));
        printf("failed to connect to database with %s = %s, %s = %s, "
                "and %s = %s\n", keywords[0], values[0], keywords[1],
                values[1], keywords[2], values[2]);
        exit_nicely(connection);
    }
    else
    {
        fprintf(stdout, "Connection SUCCESSFUL.\n");
    }

    // Create a test atomspace.
    AtomSpace* test_atomspace = new AtomSpace();

    // Generate some nodes and links.
    generate_random_atoms(test_atomspace, atom_count);

    // Dump the database.
    if (verbose)
    {
        // Get the atomspace as a string.
        std::stringstream atom_stream;
        atom_stream << *test_atomspace;

        std::cout << "Dumping atomspace: " << std::endl << std::endl;
        std::cout << atom_stream.str() << std::endl << std::endl;
    }

    // Create a new atom storage object.
    PGAtomStorage* pg_atom_storage = new PGAtomStorage(db_database, 
            db_user, db_password);

    if (!pg_atom_storage || !pg_atom_storage->connected())
    {
        if (pg_atom_storage)
            delete pg_atom_storage;
        throw RuntimeException(TRACE_INFO,
            "sql-open: Error: Unable to open the database");
    }

    // Create the new SQL backing store setting the storage.
    SQLBackingStore* backing_store = new SQLBackingStore();
    backing_store->set_store(pg_atom_storage);

    // Set verbose
    if (verbose)    
        pg_atom_storage->setVerbose();
    else
        pg_atom_storage->setVerboseOff();

    // Set SQL statement printing
    if (print_statements)   
        pg_atom_storage->setPrintStatements();
    else
        pg_atom_storage->setPrintStatementsOff();

    // Set edge storage.
    if (store_edges)    
        pg_atom_storage->setStoreEdges();
    else
        pg_atom_storage->setDontStoreEdges();

    // Set the transaction chunk if applicable.
    if (transaction_chunk > 0)
        pg_atom_storage->setTransactionChunk(transaction_chunk);

    // Delete all the existing data.
    pg_atom_storage->kill_data();

    // Reset the query count.
    pg_atom_storage->resetQueryCount();

    // Register the backing store with the atomspace.
    backing_store->registerWith(test_atomspace);

    {   // Begin timed section...
        ElapsedTimer timer("atom storage", atom_count, "store");

        // Store the atomspace into the PostgresSQL storage.
        pg_atom_storage->storeAtomSpace(test_atomspace);
    }

    std::cout << "Total store queries: " << pg_atom_storage->queryCount() <<
            std::endl;
    pg_atom_storage->resetQueryCount();

    // Test inserting atoms.
    //insert_atoms_exec(connection, DONT_USE_TRANSACTIONS, DONT_GROUP_INSERTS);
    //insert_atoms_exec(connection, USE_TRANSACTIONS, DONT_GROUP_INSERTS);
    //insert_atoms_exec(connection, DONT_USE_TRANSACTIONS, GROUP_INSERTS);
    //insert_atoms_exec(connection, USE_TRANSACTIONS, GROUP_INSERTS);

    // Unregister the backing store with the atomspace.
    backing_store->unregisterWith(test_atomspace);  

    // Now do the load.

    // Create a new atomspace.
    AtomSpace* new_atomspace = new AtomSpace();

    // Register the backing store with the new atomspace.
    backing_store->registerWith(new_atomspace);

    {   // Begin timed section...
        ElapsedTimer timer("atom loading", atom_count, "load");

        // Load the atomspace from the PostgresSQL storage.
        pg_atom_storage->loadAtomSpace(new_atomspace);
    }

    std::cout << "Total load queries: " << pg_atom_storage->queryCount() <<
            std::endl << std::endl;
    pg_atom_storage->resetQueryCount();

    // Check if this new atomspace matches.
    if (AtomSpace::compare_atomspaces(test_atomspace, new_atomspace, 
                CHECK_TRUTH_VALUES, EMIT_DIAGNOSTICS))
    {
        std::cout << "Loaded atomspace MATCHES original" << std::endl;
    }
    else
    {
        std::cout << "Loaded atomspace DOES NOT MATCH original" << std::endl 
                << std::endl;
        if (verbose)
        {
            // Get the new atomspace as a string.
            std::stringstream new_atom_stream;
            new_atom_stream << *new_atomspace;
            std::string new_atomspace_string = new_atom_stream.str();

            std::cout << "Dumping loaded atomspace: " << std::endl << std::endl;
            std::cout << new_atom_stream.str() << std::endl << std::endl;
        }
    }

    // Unregister the backing store with the atomspace.
    backing_store->unregisterWith(new_atomspace);

    // Delete the new atomspace.
    delete new_atomspace;
    new_atomspace = NULL;

    // Delete the original test atomspace.
    delete test_atomspace;
    test_atomspace = NULL;

    // Close the database connection and cleanup.
    PQfinish(connection);

    return 0;
}


#else /* HAVE_PGSQL_STORAGE */
int main () { return 1; }
#endif /* HAVE_PGSQL_STORAGE */

/* ============================= END OF FILE ================= */
