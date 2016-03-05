/*
 * FUNCTION:
 * Simple test to see if basic PostgreSQL atom storage is working.
 *
 * HISTORY:
 * Copyright (c) 2008 Linas Vepstas <linas@linas.org>
 *
 * Updated test by Curtis Faith for PostgreSQL storage - Feb 29 2016
 */

#ifdef HAVE_SQL_STORAGE

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
#include <opencog/persist/sql/PGAtomStorage.h>

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
	uuid	BIGINT PRIMARY KEY,

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
#define STATEMENT_BUFFER_SIZE (INSERT_BUFFER_SIZE + (INSERT_COUNT * VALUES_BUFFER_SIZE))

#define MICROSECONDS_PER_SEC ((double) 1000000)

#define USE_TRANSACTIONS true 
#define DONT_USE_TRANSACTIONS false

#define GROUP_INSERTS true 
#define DONT_GROUP_INSERTS false

char empty_string[] = "";
char comma_string[] = ",";



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
	fprintf(stdout, "Starting inserts ");
	print_time();
	
	// Get the starting times.
	std::chrono::steady_clock::time_point begin = std::chrono::steady_clock::now();
	clock_t start_time = clock() ;

	// Generate the insert part.
	snprintf(insert_part, INSERT_BUFFER_SIZE, "INSERT INTO Atoms "
        	"(uuid, space, type, "
        	"tv_type, stv_mean, stv_confidence, stv_count, "
        	"height, name)"
//		    "height, name, outgoing)"
        	" VALUES ");
	insert_length = strlen(insert_part);

	for (int loop = 0; loop < LOOP_COUNT; loop++)
	{
		if (use_transactions)
		{
			snprintf(statement, STATEMENT_BUFFER_SIZE, "BEGIN TRANSACTION;");
			result = PQexec(connection, statement);
			if (!result || PQresultStatus(result) != PGRES_COMMAND_OK)
			{
				fprintf(stderr, "insert_atoms_exec - BEGIN TRANSACTION failed\n");
				fprintf(stderr, "  error = %s\n", PQresultErrorMessage(result));
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
				snprintf(values_part, VALUES_BUFFER_SIZE, "%s (%lu, %lu, %u, "
			        	"%u, %.8g, %.8g, %.8g, "
			        	"%u, 'atom %lu')",
	//		        	"%u, 'atom %lu','{}');",
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
				snprintf(statement, STATEMENT_BUFFER_SIZE, "%s (%lu, %lu, %u, "
			        "%u, %.8g, %.8g, %.8g, "
			        "%u, 'atom %lu');",
	//		        "%u, 'atom %u','{}');",
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
					fprintf(stderr, "  error = %s\n", PQresultErrorMessage(result));
					PQclear(result);
				}
			}
		} // for insert

		if (use_transactions)
		{
			snprintf(statement, STATEMENT_BUFFER_SIZE, "COMMIT TRANSACTION;");
			result = PQexec(connection, statement);
			if (!result || PQresultStatus(result) != PGRES_COMMAND_OK)
			{
				fprintf(stderr, "insert_atoms_exec - COMMIT TRANSACTION failed\n");
				fprintf(stderr, "  error = %s\n", PQresultErrorMessage(result));
				PQclear(result);
			}
		}
	} // for loop

	std::chrono::steady_clock::time_point end = std::chrono::steady_clock::now();
	clock_t end_time = clock();
	fprintf(stdout,"Finished inserts ");
	print_time();

	double elapsed_time = ((double) std::chrono::duration_cast<std::chrono::microseconds> (end - begin).count()) / MICROSECONDS_PER_SEC;
	fprintf(stdout, "Elapsed time          = %12.2f seconds\n", elapsed_time);
	fprintf(stdout, "Elapsed Insert time   = %12.2f µs\n", (double) elapsed_time / (LOOP_COUNT * INSERT_COUNT) * MICROSECONDS_PER_SEC);
	fprintf(stdout, "Elapsed Inserts / sec = %9.f\n", (double) (LOOP_COUNT * INSERT_COUNT) / elapsed_time);

	elapsed_time = ((double) end_time - (double) start_time) / (double) CLOCKS_PER_SEC;
	fprintf(stdout, "CPU time              = %12.2f seconds\n", elapsed_time);
	fprintf(stdout, "CPU Insert time       = %12.2f µs\n", (double) elapsed_time / (LOOP_COUNT * INSERT_COUNT) * MICROSECONDS_PER_SEC);
	fprintf(stdout, "CPU Inserts / sec     = %9.f\n", (double) (LOOP_COUNT * INSERT_COUNT) / elapsed_time);
}

void generate_random_atoms(AtomSpace* atomspace, int total_atoms)
{
	unsigned long rand_seed = 10101010;
	RandomAtomGenerator random_atom_generator(atomspace, rand_seed);

	fprintf(stdout,"Generating random atoms ");
	print_time();
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
	fprintf(stdout,"Finished generating random atoms ");
	print_time();
}

int main (int argc, char **argv)
{
	std::string db_database = "opencog_test";
	std::string db_user = "opencog_tester";
	std::string db_password = "cheese";
	int atom_count = 100;

    const char* usage_description = 
    "PostgreSQL Storage Tester\n\n"
	"Usage: test_pg_storage[options]\n"
	"  -d <db>      \tdatabase name (default: opencog_test)\n\n"
	"  -u <user>    \tuser name (default: opencog_tester)\n\n"
	"  -p <pw>      \tpassword (default: cheese)\n\n"
    "  -s <int>     \tAtoms in test (minimum: 20, default: 100)\n\n";

    // Don't report option errors except via '?' argument
    opterr = 0;
    int c;

    // Get each command line option...
    while ((c = getopt (argc, argv, "hd:u:p:s:")) != -1) {
		switch (c)
		{
			case 'h':
				fprintf (stderr, "%s", usage_description);
				return 0;
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
					fprintf(stdout, "test_pg_storage -s option of %d not used, requires minumum 20 atoms.\n", atom_count);
					atom_count = 20;
				}
				fprintf(stdout, "Generating %d random atoms.\n", atom_count);
				break;
			case '?':
				fprintf (stderr, "%s", usage_description);
				return 0;
			default:
				fprintf (stderr, "Unknown option %c ", optopt);
				abort ();
		}
    } // while there are options

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
		printf("failed to connect to database with %s = %s, %s = %s, and %s = %s\n",
				keywords[0], values[0], keywords[1], values[1], keywords[2], values[2]);
		exit_nicely(connection);
	}
	else
	{
        fprintf(stdout, "Connection SUCCESSFUL.\n");
	}

	// Create a test atomspace.
	AtomSpace* atomspace = new AtomSpace();

	// Generate some nodes and links.
	generate_random_atoms(atomspace, atom_count);

	// Get the atomspace as a string.
	std::stringstream atom_stream;
	atom_stream << *atomspace;
	std::string original_atomspace_string = atom_stream.str();

	std::cout << "Dumping atomspace: " << std::endl << std::endl;
	std::cout << original_atomspace_string << std::endl << std::endl;

	// Create a new atom storage object.
	PGAtomStorage* pg_atom_storage = new PGAtomStorage(db_database, db_user, db_password);

	if (!pg_atom_storage || !pg_atom_storage->connected())
	{
		if (pg_atom_storage)
			delete pg_atom_storage;
		throw RuntimeException(TRACE_INFO,
			"sql-open: Error: Unable to open the database");
	}

	// Delete all the existing data.
	pg_atom_storage->kill_data();

	// Reserve the UUID range.
	pg_atom_storage->reserve();

	// Create the new SQL backing store setting the storage.
	SQLBackingStore* backing_store = new SQLBackingStore();
	backing_store->set_store(pg_atom_storage);

	// Register the backing store with the atomspace.
	backing_store->registerWith(atomspace);

	// Store the atomspace into the PostgresSQL storage.
	pg_atom_storage->storeAtomSpace(atomspace);

	// Test inserting atoms.
	//insert_atoms_exec(connection, DONT_USE_TRANSACTIONS, DONT_GROUP_INSERTS);
	//insert_atoms_exec(connection, USE_TRANSACTIONS, DONT_GROUP_INSERTS);
	//insert_atoms_exec(connection, DONT_USE_TRANSACTIONS, GROUP_INSERTS);
	//insert_atoms_exec(connection, USE_TRANSACTIONS, GROUP_INSERTS);

	// Unregister the backing store with the atomspace.
	backing_store->unregisterWith(atomspace);

    // Delete the atomspace.
    delete atomspace;
    atomspace = NULL;
 
	// Create a new atomspace.
	AtomSpace* new_atomspace = new AtomSpace();

	// Register the backing store with the new atomspace.
	backing_store->registerWith(new_atomspace);

	// Load the atomspace from the PostgresSQL storage.
	pg_atom_storage->loadAtomSpace(new_atomspace);

	// Get the new atomspace as a string.
	std::stringstream new_atom_stream;
	new_atom_stream << *new_atomspace;
	std::string new_atomspace_string = new_atom_stream.str();

	// Check if they match.
	if (new_atomspace_string == original_atomspace_string)
	{
		std::cout << "Loaded atomspace MATCHES original" << std::endl;
	}
	else
	{
		std::cout << "Loaded atomspace DOES NOT MATCH original" << std::endl << std::endl;
		std::cout << "Dumping loaded atomspace: " << std::endl << std::endl;
		std::cout << new_atomspace_string << std::endl << std::endl;
	}

	// Unregister the backing store with the atomspace.
	backing_store->unregisterWith(new_atomspace);

    // Delete the new atomspace.
    delete new_atomspace;
    new_atomspace = NULL;

    // Close the database connection and cleanup.
    PQfinish(connection);

	return 0;
}


#else /* HAVE_SQL_STORAGE */
int main () { return 1; }
#endif /* HAVE_SQL_STORAGE */

/* ============================= END OF FILE ================= */
