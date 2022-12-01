
SQL Backend Tests
-----------------
There is one subdirectory, here:

* `multi-driver`

The tests in `multi-driver` test both the `odbc` and the `postgres`
driver backends.  The `postgres` driver is about 3x faster.


HOWTO Run the SQL Tests
-----------------------

The tests in this directory require manual configuration in order to
make them run (and pass).

To run these tests, perform the following steps:

1) If you choose to change the default test database name and username,
   then edit tests/persist/sql/CMakeLists.txt and uncomment
   `SET(DB_IS_CONFIGURED 1)`.  It probably easier to just use the default
   test database name and username; see below.

2) Configure an SQL server, as described in `opencog/persist/sql/README`
   Abbreiviated instructions follow below; else skip to step 3.

2a) Install the postgres server and the postgresql client.

2b) You don't need to test ODBC, but if you are, then:
    Install `unixodbc` and `odbc-postgresql`.
    Edit `/etc/odbcinst.ini` and copy the example config into there.
    Use the Unicode drivers, not the ANSI (ASCII) drivers. Opencog
    uses unicode.

2c) Create a test database. This can be done at the shell prompt:
```
    $ createdb opencog_test
```
2d) If you get an error from the above, about missing `createdb` permissions,
    Try doing this:
```
    $ su - postgres; createuser <your-unix-username>
```
    Answer the question (yes, you want to be superuser) and exit.
    Under rare circumstances, you may need to edit `pg_hba.conf`. Google
    for additional help.

2e) Create a database user named `opencog_tester` with password `cheese`.
    You can pick a different username and password, but it must be
    consistent with the `~/.odbc.ini` file. Do NOT use your login password!
    Pick something else! Create the user at the shell prompt:
```
    $ psql -c "CREATE USER opencog_tester WITH PASSWORD 'cheese'" -d opencog_test
```
2f) Check that the above worked, by manually logging in:
```
    $  psql opencog_test -U opencog_tester -W -h localhost
```
    If you can't login, something up above failed.

2g) Initialize the test database by creating tables. Do this with:
```
    $ cat opencog/persist/sql/odbc/atom.sql | psql opencog_test -U opencog_tester -W -h localhost
```
2h) Verify that the tables were created. Login as before, then enter
    `\d` at the postgres prompt.  You should see this:
```
    opencog_test=> \d
                  List of relations
     Schema |    Name    | Type  |     Owner
    --------+------------+-------+----------------
     public | atoms      | table | opencog_tester
     public | spaces     | table | opencog_tester
     public | typecodes  | table | opencog_tester
     public | valuations | table | opencog_tester
    (4 rows)
```
    If the above doesn't work, go back, and try again.

2i) Verify that `opencog_tester` has write permissions to the tables. Do
    this by entering the below.
```
    opencog_test=> INSERT INTO TypeCodes (type, typename) VALUES (97, 'SemanticRelationNode');
```
    You should see the appropriate respone:
```
    INSERT 0 1
```
    If the above doesn't work, go back, and try again.

    One "typical" reason for failure at this point is that the tables
    aren't owned by `opencog_tester`. There are two ways to fix this:
    drop the tables, and create them again with the right owner, or
    issue the SQL statement
```
        ALTER TABLE typecodes OWNER TO opencog_tester;
```
    and likewise for the other three tables.

2j) Only if testing ODBC:
    Edit `~/.odbc.ini` and add something similar to the below. Pay special
    attention to the username, the password, and the name of the database
    to connect to.
```
    [opencog_test]
    Description = Unit-Test DB for Opencog unit tests.
    Driver      = PostgreSQL
    Trace       = 0
    TraceFile   =
    CommLog     = No
    Database    = opencog_test
    Servername  = localhost
    Port        = 5432
    Username    = opencog_tester
    Password    = cheese
    ReadOnly    = No
    RowVersioning     = No
    ShowSystemTables  = Yes
    ShowOidColumn     = Yes
    FakeOidIndex      = Yes
    ConnSettings      =
```

2k) Edit `lib/atomspace-test.conf` and verify that the username and password
    are set as above.

3) Run the five test cases:

```
   $ ./tests/persist/sql/multi-driver/BasicSaveUTest
   $ ./tests/persist/sql/multi-driver/ValueUTest
   $ ./tests/persist/sql/multi-driver/PersistUTest
   $ ./tests/persist/sql/multi-driver/MultiPersistUTest
   $ ./tests/persist/sql/multi-driver/FetchUTest
```
   It should print `OK!` at the end, if all tests passed.

3a) If the above gives the message
```
    Error: Test failed: Cannot connect to database: FATAL:  Peer authentication failed for user "opencog_tester"
```
    then edit
```
    /etc/postgresql/9.3/main/pg_hba.conf
```
    and add the line
```
    local   all      all           md5
```
    Be sure to reload or restart the postgres server after the above change:
```
   service postgresql reload
```


3b) If the above doesn't work, note that the `lib/atomspace-test.conf`
    being used is the one in the BUILD directory not the SOURCE directory!

3c) If the above still doesn't work, make sure that postgres is actually
    listening on port 5432, and not some other port.  If it is using a
    different port, and testing ODBC, then change `~/.odbc.ini` to that.
    The postgres config can be found in
```
    /etc/postgresql/9.6/main/postgresql.conf
```
    look for "port =" near the beginning of the file.
