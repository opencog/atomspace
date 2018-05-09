;
; OpenCog SQL Persistance module
;

(define-module (opencog persist-sql))

(use-modules (opencog))

(load-extension "libpersist-sql" "opencog_persist_sql_init")

(export sql-clear-cache sql-clear-stats sql-close sql-load sql-open
	sql-store sql-stats sql-set-hilo-watermarks! sql-set-stall-writers!)

(set-procedure-property! sql-clear-cache 'documentation
"
 sql-clear-cache - clear the TLB of cached data
    This will free up RAM, maybe a lot, dependeing on how many atoms
    are in the cache. The cost of doing this is that some operations
    on atoms will no longer be cached, and will need to go to the
    database to fetch contents, potentially impacting performance.
")

(set-procedure-property! sql-clear-stats 'documentation
"
 sql-clear-stats - reset the performance statistics counters.
    This will zero out the various counters used to track the
    performance of the SQL backend.  Statistics will continue to
    be accumulated.
")

(set-procedure-property! sql-close 'documentation
"
 sql-close - close the currently open SQL backend.
    Close open connections to the currently-open backend, afterflushing
    any pending writes in the write queues. After the close, atoms can
    no longer be stored to or fetched from the database.
")

(set-procedure-property! sql-load 'documentation
"
 sql-load - load all atoms in the database.
    This will cause ALL of the atoms in the open database to be loaded
    into the atomspace. This can be a very time-consuming operation.
    In normal operation, it is rarely necessary to load all atoms;
    atoms can always be fetched and stored one at a time, on demand.
")

(set-procedure-property! sql-open 'documentation
"
 sql-open URL - Open a connection to a database
    Open a connection to the database encoded in the URL. All
    appropriate database credentials must be supplied in the URL,
    including the username and password, if required.

    The URL must be on one of these formats:
       odbc://USER:PASSWORD/DBNAME
       postgres://USER@HOST/DBNAME
       postgres://USER:PASSWORD@HOST/DBNAME
       postgres:///DBNAME?user=USER
       postgres:///DBNAME?user=USER&host=HOST
       postgres:///DBNAME?user=USER&password=PASS

    Other key-value pairs following the question-mark are interpreted
    by the postgres driver, according to postgres documentation.

  Examples of use with valid URL's:
     (sql-open \"odbc://opencog_tester:cheese/opencog_test\")
     (sql-open \"postgres://opencog_tester@localhost/opencog_test\")
     (sql-open \"postgres://opencog_tester:cheese@localhost/opencog_test\")
     (sql-open \"postgres:///opencog_test?user=opencog_tester\")
     (sql-open \"postgres:///opencog_test?user=opencog_tester&host=localhost\")
     (sql-open \"postgres:///opencog_test?user=opencog_tester&password=cheese\")
")

(set-procedure-property! sql-set-hilo-watermarks! 'documentation
"
 sql-set-hilo-watermarks! HI LO - Set the high and low watermarks on the
    writeback queues.  Any threads that are storing atoms will block
    if the backlog of unwritten atoms exceeds the high watermark.
    The threads will unblock once the queues drain below the low
    watermark level.
")

(set-procedure-property! sql-set-stall-writers! 'documentation
"
 sql-set-stall-writers! BOOL - Set the stall flag on the writeback
    queues. If the flag is set, then the writers will 'stall', i.e.
    avoid doing any actual stores until the writeback queues have
    at least the low-watermark pending writes in them.
")

(set-procedure-property! sql-store 'documentation
"
 sql-store - Store all atoms in the atomspace to the database.
    This will dump the ENTIRE contents of the atomspace to the databse.
    Depending on the size of the database, this can potentially take a
    lot of time.  During normal operation, a bulk-save is rarely
    required, as individual atoms can always be stored, one at a time.
")

(set-procedure-property! sql-stats 'documentation
"
 sql-stats - report performance statistics.
    This will cause some database performance statistics to be printed
    to the stdout of the server. These statistics can be quite arcane
    and are useful primarily to the developers of the database backend.
")
