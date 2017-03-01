SQL Persist
===========
+ Linas Vepstas <linasvepstas@gmail.com>
+ Basic implementation Feb-June 2008
+ Status update May 2009
+ Status update Dec 2013
+ Status update Jan 2017

A simple implementation of atom persistence in SQL.  This allows not
only saving and restoring of the atomspace, but it also allows multiple
cogservers to share a common set of data.  That is, it implements a
basic form of a distributed atomspace.

Status
======
It works and has been used with databases containing millions of atoms,
accessed by cogservers that ran for months to perform computations. It
has scaled trouble-free, without any slowdown, up to four cogservers.
No one has tried anything larger than that, yet.

Features
--------
 * Save and restore of individual atoms, and several kinds of truth values.
 * Bulk save-and-restore of entire AtomSpace contents.
 * Incremental save/restore (i.e. update the SQL contents as AtomSpace
   changes).
 * Generic API, useful for inter-server communications.

Missing features/ToDo items
---------------------------
 * Add support for multiple atom spaces.
 * Provide optimized table layout for EvaluationLinks.
 * Add support for Space/TimeServer data.
 * Implement ProtoAtom storage.
 * See also TODO list at very bottom.

Performance status
------------------
In March 2014, 10.3M atoms were loaded in about 20 minutes wall-clock
time, 10 minutes of opencog-server CPU time.  This works out to about
500K atoms/minute, or 9K atoms/second.  The resulting cogserver required
about 10GBytes of RAM, which works out to about 1KByte/atom average.
The loaded hypergraphs were all EvaluationLinks, viz:
```
   EvaluationLink  (w/non-trivial TV)
      SomeNode
      ListLink
         WordNode  (w/ non-trivial TV)
         WordNode  (w/ non-trivial TV)
```

The above measurements were made on a busy server that was doing many
other CPU & RAM intensive things; there was no performance tuning of
the Postgres server.  A section below explains how to performance tune
the Postgres server for better results.  The above was also done through
the scheme interface; since then, garbage collection has been tuned a
little bit, and so RAM usage should drop a bit.


Design Goals
============
The goal of this implementation is to:

1) Provide OpenCog with a working memory, so that the Cogsever could
   be stopped and restarted without requiring a data dump.  That is,
   checkpointing should be possible: if the cogserver crashes, data is
   not lost.  By using a database, a file format does not need to be
   invented. By using a database, data integrity is assured.
   By using incremental access, only those atoms that are needed get
   loaded into the atomspace; one does NOT have to do a bulk restore
   if one doesn't want to.

2) Provide an API for inter-server communications and atom exchange.
   Multiple cogservers can share data simply by sending atoms to,
   and retrieving atoms from the database.  Although this may not be
   the fastest way to send just single atoms, most algorithms do not
   need to send just single atoms: they just need to share some atoms,
   but its not clear which ones need to be shared.  Since all atoms are
   in the database, only the ones that are needed can be retrieved.

3) Provide a baseline/reference implementation by which other
   persistence designs can be measured. It is hoped that other systems
   would be at least as fast and as scalable as this one is: this is
   meant to provide a minimal function and performance level. The
   strength of the current design is supposed to be simplicity, not
   scalability or raw performance.

4) A non-design-goal (at this time) is to build a system that can scale
   to more than 100 cogserver instances.  The current design might be
   able to scale to this many, but probably not much more.  Scaling
   larger than this would probably require a fundamental redesign of
   all of opencog, starting with the atomspace.

5) A non-design-goal is fully automatic save-restore of atoms.  Both
   the save and restore of atoms must be triggered by calls to the
   atomspace API.  The reason for this design point is that the
   atomspace is at too low a level to be fully automatic.  It cannot
   guess what the user really wants to do.  If it did guess, it would
   probably guess wrong: saving atoms before the user is done with them,
   saving atoms that get deleted microseconds later, fetching atoms
   that the user is completely disinterested in, clogging up RAM and
   wasting CPU time.  Some other layer, a higher level layer, needs to
   implement a policy for save/restore.  This layer only provides a
   mechanism.  It would be very very wrong to implement an automatic
   policy at this layer.


Current Design
==============
The core design defines only a few very simple SQL tables, and some
readers and writers to save and restore atoms from an SQL database.

Note that the core design does *not* make use of object reflection,
nor can it store arbitrary kinds of objects. It is very definitely
hard-wired. Yes, this can be considered to be a short-coming.
A more general, persistent object framework (for C) can be found
at http://estron.alioth.debian.org/  However, simplicity, at the
cost of missing flexibility, seems more important.

The current design can save/restore individual atoms, and it can
bulk-save/bulk-restore the entire contents of an AtomTable.
A semi-realized goal of the prototype is to implement incremental save
and restore -- that is, to fetch atoms in a "just in time" fashion, and
to save away atoms that are not needed in RAM (e.g. atoms with
low/non-existent attention values). The AtomSpace BackingStore provides
the current, minimalistic, low-function API for this.

Features
--------
 * The AtomStorage class is thread-safe, according to the unit tests
and limited personal experience.

 * Fully automated mapping of in-RAM atoms to in-storage universal
unique identifiers (UUID's), using the TLB mechanism.

 * Multi-user issuance and management of UUID's is un-tested and is
probably broken at this time.  That is, if multiple cogservers connect
to the atomspace at the same time, it probably won't work.  It used to
work, but there have been some major changes that probably broke this.
There is existing infrastructure that enables this, e.g. one can
"malloc" ranges of UUID's.  The code has bit-rotted, for lack of use.

 * This implementation automatically handles clashing atom types.  That
is, if the database is written with one set of atom types, and then the
cogserver is stopped, the atomtypes are all changed (with some added,
some deleted), then pulling from the database will automatically
translate and use the new atom types. (The deleted atom types will not
be removed from the database.  Restoring atoms with deleted atomtypes
will cause an exception to be thrown.)

 * Non-blocking atom store requests are implemented.  Four asynchronous
write-back threads are used, with hi/lo watermarks for queue management.
That is, if a user asks that an atom be stored, then the atom will be
queued for storage, and one of these threads will perform the actual
store at a later time. Meanwhile, the store request returns to the user
immediately, so that the user can continue without blocking.  The only
time that an atom store request blocks is if the queue is completely
full; in that case, the user will be blocked until the queue drains
below the low watermark. The number of connections can be raised by
editing the AtomStorage constructor, and recompiling.

The fire-n-forget algo is implemented in the C++
`AtomStorage::storeAtom()` method.  If the backlog of unwritten atoms
gets too large, the storeAtom() method may stall. Its currently designed
to stall if there's a backlog of 100 or more unwritten atoms.  This can
be changed by searching for `HIGH_WATER_MARK`, changing it and recompiling.

 * Reading always blocks: if the user asks for an atom, the call will
not return to the user until the atom is available.  At this time,
pre-fetch has not been implemented.  But that's because pre-fetch is
easy: the user can do it in their own thread :-)


Install, Setup and Usage HOWTO
==============================
There are many steps needed to install and use this. Sorry!

07-05-2015: Updated instructions below to be slightly more instructive
	    and up-to-date for PostgreSQL 9.3 and Ubuntu 14.04.

Compiling
---------
Download and install the `libpq-dev` packages, which provide the
C-language bindings to the Postgres client library.

```
  sudo apt-get install libpq-dev
  cmake
  make
```

Optional ODBC drivers
---------------------
Optionally, download and install UnixODBC devel packages.
Do NOT use IODBC, it fails to support UTF-8.
The Postgres drivers seem to be considerably faster; the use of the
ODBC driver is discouraged, unless you really need it or really like it.

```
  sudo apt-get install unixodbc-dev odbc-postgresql
  cmake
  make
```

Database Setup
--------------
There are four basic steps needed to setup the database: installation,
device driver setup, user and password setup, and table initialization.
Each step discussed below.


Database Install
----------------
Download and install Postgres version 9.3 or newer.  The current design
simply won't work with MySQL, because of a lack of array support.
Same holds true for SQLite.  Sorry. There is some work in the
code-base to support these other databases, but the work-arounds for
the missing features are kind-of complicated, and likely to be slow.

Be sure to install the Postgres server and the Postgres client.

```
    sudo apt-get install postgresql-9.3
```

Optional: ODBC Device Driver Setup
----------------------------------
If you want to use ODBC, then you need to configure the ODBC driver.
Again: the use of ODBC is optional and discouraged; its slower clunkier
and more complex.

After install, verify that `/etc/odbcinst.ini` contains the stanza
below (or something similar).  If it is missing, then edit this file
(as root) and add the stanza.  Notice that it uses the Unicode drivers,
and NOT the ANSI (ASCII) drivers.  Opencog uses Unicode!

```
    sudo vi /etc/odbcinst.ini &
```

```
    [PostgreSQL Unicode]
    Description = PostgreSQL ODBC driver (Unicode version)
    Driver      = psqlodbcw.so
    Setup       = libodbcpsqlS.so
    Debug       = 0
    CommLog     = 0
```

The above stanza associates the name `PostgreSQL Unicode` with a particular
driver. This name is needed for later steps.  Notice that this is a quite
long name, with spaces!  You can change the name, (e.g. to shorten it) if
you wish, however, it **MUST** be consistent with the name given in the
`.odbc.ini` file (explained below in 'ODBC Setup, Part the Second').

MySQL users need the stanza below; the `/etc/odbcinst.ini` file can
safely contain multiple stanzas defining other drivers.

WARNING: MySQL is currently not supported. Anyway, if you wanted to
mess with it, then add the below:
```
    [MySQL]
    Description = MySQL driver
    Driver      = libmyodbc.so
    Setup       = libodbcmyS.so
    CPTimeout   =
    CPReuse     =
```

Performance tweaks
------------------
The Postgres default configuration can be/should be tweaked for
performance.  Newer version of Postgres seem to be OK (??) but in
some cases, performance will be a disaster if the database is not
tuned.

Edit `postgresql.conf` (a typical location is
`/etc/postgresql/9.3/main/postgresql.conf`) and make the changes below.
The first two changes are recommended by
http://wiki.postgresql.org/wiki/Tuning_Your_PostgreSQL_Server
```
   shared_buffers = default was 32MB, change to 25% of install RAM
   work_mem = default was 1MB change to 32MB
   effective_cache_size = default was 128MB, change to 50%-75% of installed RAM
   fsync = default on  change to off
   synchronous_commit = default on change to off
   wal_buffers = default 64kB change to 2MB or even 32MB
   commit_delay = default 0 change to 10000 (10K) microseconds
   checkpoint_segments = 32 (each one takes up 16MB disk space)
   ssl = off
   autovacuum = on
   track_counts = on
```
A large value for `wal_buffers` is needed because much of the database
traffic consists of updates.  Enabling vacuum is very important, for
the same reason; performance degrades substantially (by factors of
3x-10x) without regular vacuuming. (Newer versions of Postgres vacuum
automatically. YMMV.)

Restarting the server might lead to errors stating that max shared mem
usage has been exceeded. This can be fixed by telling the kernel to use
6.4 gigabytes (for example):
```
   vi /etc/sysctl.conf
   kernel.shmmax = 6440100100
```
save file contents, then:
```
   sysctl -p /etc/sysctl.conf
```
If you continue to get errors after that, read this for help fixing them:

   http://stackoverflow.com/questions/12616935/postgresql-shared-memory-settings


Database Setup
--------------
A database to hold the opencog data needs to be created.  Multiple
databases can be created.  In this example, the database name will
be "mycogdata".  Change this as desired.  The unit tests require a
scratch database called "opencog_test".

So, at the Unix command line:

```
   $ createdb mycogdata
```

If you get an error message `FATAL:  role "<user>" does not exist`, then
try doing this, replacing 'alex' with your username.

```
   $ sudo su - postgres
   $ psql template1
   template1=# CREATE ROLE alex WITH SUPERUSER;
   template1=# ALTER ROLE alex WITH LOGIN;
```

Verify that worked out by typing \dg to see:

                             List of roles
 Role name |                   Attributes                   | Member of
-----------+------------------------------------------------+-----------
 alex      | Superuser                                      | {}
 postgres  | Superuser, Create role, Create DB, Replication | {}

Then do Ctrl+D to exit, ignoring any message about `psql_history`, and
return to your own account:

```
   $ exit
```

If you ran into the error above you still need to create the database of
course (no output if successful):

```
   $ createdb mycogdata
```
You should be able to access that database using the so-called `peer`
authentication method. This command:
```
   $  psql mycogdata
```
should place you at the postgres-client prompt; you are now ready to
create the opencog tables.  If the above didn't work, then you will
have to create an explicit database user login, as explained below;
otherwise, creating a user is optional.


User setup
----------
(Optional)  You can continue without creating a database user, if you
wish; postgres automatically provides password-less `peer`
authentication to your unix username. However, the unit-tests do require
that a distinct user be created, called `opencog_tester`, although you
can bypass this, too, by altering the opencog test configuration file.

The database user is NOT the same thing as a unix user: the login is for
the database (only), not the OS. In general, you will want to pick a
password that is DIFFERENT than your unix-password. This is because some
of the database login methods require a clear-text password to be
supplied. The full set of postgres login styles are supported, as long
as you use the postgres URI format.  The ODBC logins require cleartext
passwords to be used.

See the [postgres
documentation]((https://www.postgresql.org/docs/9.6/static/libpq-connect.html)
for details on the allowed URI format.

In the following, a database user named `opencog_user` is created, and
given the password `cheese`.  You can pick a different username and
password.  If you are using ODBC (not recommended), then these two must
be consistent with the `~/.odbc.ini` file. Do NOT use your unix password!
Pick something else! Create the user at the shell prompt:

```
   $ psql -c "CREATE USER opencog_user WITH PASSWORD 'cheese'" -d mycogdata
```

Check that the above worked, by manually logging in:

```
   $  psql mycogdata -U opencog_user -W -h localhost
```

If you can't login, something up above failed.

Next, see if you can login using unix-domain sockets, instead of
TCP-IP sockets to `localhost`:

```
   $  psql mycogdata -U opencog_user
```

For most users, this will fail with the message
```
psql: FATAL:  Peer authentication failed for user "opencog_user"
```
To fix this, you need to edit the file
```
   /etc/postgresql/9.3/main/pg_hba.conf
```
as unix user `postgres`, and add the following line. It will allow
password logins for all local users. (A local user is a user on the
same machine, connecting to the database by means of unix-domain
sockets, instead of tcp/ip sockets.)
```
   local   all      all           md5
```
Be sure to reload or restart the postgres server after the above change:
```
   service postgresql reload
```

Table initialization
--------------------
Next, you need to populate the database with OpenCog-specific tables.
Navigate to the atomspace folder you cloned from GitHub:

```
   $  cd ~/atomspace
```

Create the database tables:

```
   $ cat opencog/persist/sql/postgres/pg_atom.sql | psql mycogdata
```

If you are using a different user-id than your login, then you will
have to add the `-U opencog_user` flag to the `psql` command.  If you
created a distinct user, and did not set up the `hba.conf` file as
above, then you also need a `-h localhost` flag, to access the database
using TCP/IP sockets on the local network.

Verify that the tables were created. Login as before:

```
   $  psql mycogdata
```

Then enter `\d` at the postgres prompt.  You should see this:

```
    mycogdata=> \d
                  List of relations
     Schema |   Name    | Type  |     Owner
    --------+-----------+-------+----------------
     public | atoms     | table | opencog_user
     public | spaces    | table | opencog_user
     public | typecodes | table | opencog_user
    (3 rows)
```

If the above doesn't work, go back, and try again.

Verify that you have write permissions to the tables. Do this by
entering the below.

```
    mycogdata=> INSERT INTO TypeCodes (type, typename) VALUES (97, 'SemanticRelationNode');
```

You should see the appropriate response:

```
    INSERT 0 1
```

If the above doesn't work, go back, and try again. Exit with Ctrl+D.


ODBC Setup, Part the Second
---------------------------
(Optional, required only if you are using the deprecated ODBC bindings.)
Edit `~/.odbc.ini` in your home directory to add a stanza of the form
below. You MUST create one of these for EACH repository you plan to
use! The name of the stanza, and of the database, can be whatever you
wish. The name given for `Driver`, here `PostgreSQL Unicode`, **must**
match a stanza name in `/etc/odbcinst.ini`.  Failure to have it match
will cause an error message:

```
Can't perform SQLConnect rc=-1(0) [unixODBC][Driver Manager]Data source name not found, and no default driver specified
```

Note that below specifies a username and a password. These should NOT
be your regular unix username or password!  Make up something else,
something completely different! These two will be the username and the
password that you use when connecting from the cogserver, with the
`sql-open` command (below).

Pay special attention to the name given for the `Database`.  This should
correspond to a database created with postgres. In the examples above,
it was `mycogdata`.

IMPORTANT: MAKE SURE THERE ARE NO SPACES AT THE START OF EVERY LINE!
           IF YOU COPY-PASTE THE TEXT BELOW YOU WILL HAVE SPACES!
           REMOVE THEM! OR YOU WILL ALSO GET THE ERROR ABOVE!

```
   [mycogdata]
   Description       = My Favorite Database
   Driver            = PostgreSQL Unicode
   CommLog           = No
   Database          = mycogdata
   Servername        = localhost
   Port              = 5432
   Username          = opencog_user
   Password          = cheese
   ReadOnly          = No
   RowVersioning     = No
   ShowSystemTables  = Yes
   ShowOidColumn     = Yes
   FakeOidIndex      = Yes
   ConnSettings      =
```

Opencog Setup
-------------
Edit `~/opencog/build/lib/opencog.conf` and set the `STORAGE`,
`STORAGE_USERNAME` and `STORAGE_PASSWD` there to the same values as
in `~/.odbc.ini`.

```
STORAGE               = "mycogdata"
STORAGE_USERNAME      = "opencog_user"
STORAGE_PASSWD        = "cheese"
```

Or copy lib/opencog.conf to your build directory, edit the copy, and
start the opencog server from your build folder as:

```
   $ ./opencog/server/cogserver -c my.conf
```

Verify that everything works. Start the cogserver, and bulk-save.
(Actually this didn't work for me, I had to do sql-open before sql-store
worked, as shown below, even though my opencog.conf was correct and when
using a custom my.conf file.)

```
   $ telnet localhost 17001
   opencog> sql-store
```

Some output should be printed in the COGSERVER window:

```
   Max UUID is 57
   Finished storing 55 atoms total.
```

The typical number of atoms stored will differ from this.

You don't have to put the database credentials into the `opencog.conf`
file.  In this case, the database would need to be opened manually,
using the `sql-open` command:

```
   $ telnet localhost 17001
   `opencog> sql-open mycogdata opencog_user cheese
```

Notice that the user-name and the password are the same as that given
in the `~/.odbc.ini` file.  These are NOT (and should not be) your unix
username and password!


How To Pass the Unit Tests
==========================
There are four unit tests for this API, `BasicSaveUTest`, `PersistUTest`,
`MultiPersistUTest` and `FetchUTest`.  The CMakefile will not compile or
run these tests unless you have either:
* created an `opencog_tester` user and an `opencog_test` database, or
* altered the `lib/opencog-test.conf` file, and placed a different
  user/database login combination in there.

To run and past these tests, you should:

 * Read tests/persist/sql/README and follow the instructions there.
   They are almost identical to the instructions above, except that
   they require a user called `opencog_tester` to be set up. You can
   do that, or alter the config file to use your current DB user.
 * To compile and run:
```
    $ make test
```

So here's a super-short recap:

 * Create a user 'opencog_tester' with password 'cheese'.
 * Create a new database: e.g.  `$ createdb opencog_test`
 * Create the tables: `$ cat atom.sql | psql `
 * (Optional) Create an entry in `~/.odbc.ini`, as explained above.
   The entry should be called `opencog_test`, and use `opencog_tester`
   as the user. Password is `cheese`.
 * The file `lib/opencog-test.conf` already has the above as the default
   username and database names.  Stick to these.


After the above steps, `BasicSaveUTest`, `PersistUTest`,
`MultiPersistUTest` and `FetchUTest` should run and pass.


Unit Test Status
----------------
* As of 2011-04-29 bzr revision 5314, BasicSaveUTest (still) passes.
* As of 2013-11-26 BasicSaveUTest works and passes, again.
* As of 2014-06-19 both unit tests work and pass.
* As of 2015-04-23 both unit tests work and pass.
* As of 2017-01-20 all four unit tests work and pass.


Using the System
================
Some example walkthroughs of some typical usage scenarios.

Access via guile
----------------
Start the guile shell, and load the needed modules:
```
$ guile
scheme@(guile-user)> (use-modules (opencog))
scheme@(guile-user)> (use-modules (opencog persist) (opencog persist-sql))
```
Open a database with `sql-open`:
```
scheme@(guile-user)> (sql-open "postgres:///opencog_test?user=opencog_tester")
Reserving UUID up to 3
```
There are other, alternate forms for the URI. See the [postgres
documentation](https://www.postgresql.org/docs/9.6/static/libpq-connect.html)
for details.
```
> (sql-open "odbc://opencog_tester:cheese/opencog_test")
> (sql-open "postgres://opencog_tester@localhost/opencog_test")
> (sql-open "postgres://opencog_tester:cheese@localhost/opencog_test")
> (sql-open "postgres:///opencog_test?user=opencog_tester")
> (sql-open "postgres:///opencog_test?user=opencog_tester&host=localhost")
> (sql-open "postgres:///opencog_test?user=opencog_tester&password=cheese")
```

Save an atom with `store-atom`:
```
scheme@(guile-user)> (define x (ConceptNode "asdfasdf" (stv 0.123 0.789)))
scheme@(guile-user)> (store-atom x)
```

Other useful scheme functions: `fetch-atom` and `fetch-incoming-set`.
A debugging print: `sql-stats`

Bulk load and restore: `sql-load` `sql-store` `sql-close`


Bulk Save and Restore
---------------------
Bulk save and restore of atoms can also be done from the cogserver
command line -- `rlwrap telnet -8 localhost 17001` and issuing the
commands:
```
    opencog> ?
    Available commands:
      exit help list scm shutdown sql-open sql-close sql-store sql-load
    opencog> sql-open
    sql-open: invalid command syntax
    Usage: sql-open <dbname> <username> <auth>

    opencog> sql-open mycogdata opencog_user cheese
    Opened "mycogdata" as user "opencog_user"

    opencog> sql-store
    SQL data store thread started
```
At this point, a progress indicator will be printed by the opencog
server, on the OpenCog server's stdout. It  will say something like:
Stored 236000 atoms. When finished, its nice to say:
```
    opencog> sql-close
```
At this point, the cogserver can be stopped and restarted.  After a
restart, load the data with:
```
    opencog> sql-open mycogdata opencog_user cheese
    opencog> sql-load
    SQL loader thread started
```
The completion message will be on the server output, for example:
```
    Finished loading 973300 atoms in total
```

Individual-atom save and restore
--------------------------------
Individual atoms can be saved and fetched, using the guile interface.
Either the guile shell, or the cogserver shell can be used.  If the
guile shell is used, then you do NOT! need to start the cogserver!

```
    guile> (define x (ConceptNode "asdfasdf" (stv 0.123 0.789)))
    guile> (store-atom x)
    (ConceptNode "asdfasdf" (stv 0.123 0.78899997))
```
The above will have caused a single atom to be stored in the database.
It's presence can be verified by examining the database directly:
```
    $ psql mycogdata
    Welcome to psql 8.3.6, the PostgreSQL interactive terminal.
    mycogdata=# select * from atoms;
     uuid | type | tv_type | stv_mean | stv_confidence | stv_count | height |   name   | outgoing
    ------+------+---------+----------+----------------+-----------+--------+----------+----------
        2 |    3 |       1 |    0.123 |     0.78899997 | 2991.4688 |      0 | asdfasdf |
    (1 row)
```
The backing-store mechanism can now retrieve this atom at a later time.
Thus, for example, exit, restart, and re-open the database again. Then,
```
    guile> (ConceptNode "asdfasdf")
    (ConceptNode "asdfasdf")

    guile> (cog-fetch (ConceptNode "asdfasdf"))
    (ConceptNode "asdfasdf" (stv 0.123 0.78899997))
```
When the atom is first recreated, it just has the default TV on it. The
TV is not updated until it is explicitly fetched. The existing TV on
the atom is clobbered when it is fetched; no attempt is made to merge.

If the truth value is modified, the atom is *not* automatically saved
to the database.  The modified atom would need to be explicitly saved
again.

Once stored, the atom may be deleted from the AtomSpace; it will
remain in storage, and can be recreated at will:
```
    guile> (cog-extract y)
    #t
    guile> y
    #<Invalid handle>
    guile> (define y (ConceptNode "asdfasdf"))
    guile> (cog-fetch y)
    (ConceptNode "asdfasdf" (stv 0.123 0.78899997))
```
Notice, in the above, that the truth value is the same as it was before.
That is because the truth value was fetched from the database when the
atom is recreated.


Extraction vs. Deletion
-----------------------
There are four related but distinct concepts of atom deletion: extract
and delete, each of which may also be done recursively. "Extract" will
remove the atom from the (local, in-RAM) atomspace; it will NOT remove
it from the database!  "Delete" will remove the atom from both the
atomspace, and from the database; thus, deletion is permanent!
(Caution: there are pathological situations. For example, if there are
two atomspaces attached to one database, and delete is used in one
atomspace (thus deleting the atom in the database), there may still be
a copy in the second atomspace.  If the second atomspace stores that
atom, it will be re-created in the database.  There is no way to
broadcast a distributed system-wide delete message.  Such a message
does not seem to be a good idea, anyway.)

Atoms can also be extracted or deleted recursively. Thus, normally, a
extract/delete will succeed only if the atom has no incoming set.
The recursive forms, `cog-extract-recursive` and `cog-delete-recursive`
extract/delete the atom and every link that contains that atom, and so
on.


Using SQL Persistence from Scheme
---------------------------------
SQL fetch from store is not automatic!  That is because this layer
cannot guess the user's intentions. There is no way for this layer
to guess which atoms might be deleted in a few milliseconds from now,
or to guess which atoms need to loaded into RAM (do you really want ALL
of them to be loaded ??)  Some higher level management and policy will
need to make the fetch and store decisions. This layer only implements
the raw capability: it does not implement the policy.

Incoming sets of an atom can be fetched by using either C++:
```
   AtomSpace::fetchIncomingSet(h, true);
```
or the scheme call
```
  (fetch-incoming-set atom)
```

To force an atom to be saved, call:
```
   (store-atom atom)
```

Copying Databases
-----------------
Sooner or later you will want to make a copy of your database, for
backup purposes, or sharing. Here's the current 'best practices' for
that:
```
   $ pg_dump mycogdata -f filename.sql
```
That's all!



Experimental Diary & Results
============================
Diary entries from June 2008

Store performance
-----------------
This section reviews the performance for storage of data from opencog
to the SQL server (and thence to disk).  Performed in 2008, on a typical
Intel desktop that was new in 2004. Viz. under two GhZ, and 4GB RAM.

First run with a large data set (save of 1564K atoms to the database)
was a disaster.  Huge CPU usage, with 75% of CPU usage occurring in the
kernel block i/o layer, and 12% each for the opencog and postgres times:
```
   112:00 [md4_raid1] or 4.3 millisecs per atom
   17 minutes for postgres, and opencog, each. or 0.66 millisecs per atom
   1937576 - 1088032 kB = 850MBytes disk use
```
Experiment: is this due to the bad design for trying to figure whether
"INSERT" or "UPDATE" should be used? A local client-side cache of the
keys in the DB seems to change little:
```
   CPU usage for postgres 11:04  and opencog 10:40 and 112:30 for md
```
So this change drops postgres server and opencog CPU usage
significantly, but the insane kernel CPU usage remains.

The above disaster can be attributed to bad defaults for the postgres
server. In particular, sync to disk, while critical for commercial
database use, is pointless for current use. Also, buffer sizes are much
too small. Edit postgresql.conf and make the following changes:
```
   shared_buffers = default was 24MB, change to 384MB
   work_mem = default was 1MB change to 32MB
   fsync = default on  change to off
   synchronous_commit = default on change to off
   wal_buffers = default 64kB change to 512kB
   commit_delay = default 0 change to 10000 (10K) microseconds
   ssl = default true change to false
```
Restarting the server might lead to errors stating that max shared mem
usage has been exceeded. This can be fixed by:
```
   vi /etc/sysctl.conf
   kernel.shmmax = 440100100
(save file contents, then)
   sysctl -p /etc/sysctl.conf
```
After tuning, save of data to empty DB gives result:
```
   cogserver = 10:45 mins = 0.41  millisecs/atom (2.42K atoms/sec)
   postgres  =  7:32 mins = 0.29  millisecs/atom (2.65K atoms/sec)
   md        =  0:42 mins = 0.026 millisecs/atom (37K atoms/sec)
```
Try again, dropping the indexes on the atom and edge tables. Then,
after loading all atoms, rebuild the index. This time, get
```
   cogserver = 5:49 mins = 0.227 millisecs/atom (4.40K atoms/sec)
   postgres  = 4:50 mins = 0.189 millisecs/atom (5.30K atoms/sec)
```
Try again, this time with in-line outgoing sets. This improves
performance even further:
```
   cogserver = 2:54 mm:ss = 0.113 millisecs/atom (8.83K atoms/sec)
   postgres  = 2:22 mm:ss = 0.092 millisecs/atom (10.82K atoms/sec)
```
Try again, compiled with -O3, storing to an empty table, with
no indexes on it (and with in-line outgoing sets):
```
   cogserver = 2:40 mm:ss
   postgres  = 2:16 mm:ss
```
Try again, compiled with -O3, storing to empty table, while holding
the index on tables (and with in-line outgoing sets).
```
   cogserver = 2:51 mm:ss
   postgres  = 2:06 mm:ss
```
Apparently, the problem with the indexes has to do with holding them
for the edge table; when there's no edge table, then there's no index
issue!?

Try again, compiled with -O3, saving to (updating) a *full* database.
(i.e. database already has the data, so we are doing UPDATE not INSERT)
```
   cogserver = 2:19 mm:ss
   postgres  = 4:35 mm:ss
```
Try again, using UnixODBC instead of iODBC, to empty table, withOUT
index tables (and -O3 and in-lined outgoing):
```
   cogserver = 2:36 mm:ss
   postgres  = 2:13 mm:ss
```
It appears that UnixODBC is essentially identical to iODBC performance
```
begin; commit;
use analyze;
use prepare;
```

XML loading performance
-----------------------
Loading the dataset from XML files takes:
```
   cogserver 2:34 mm:ss when compiled without optimization
   cogserver 1:19 mm:ss when compiled with -O3 optimization
```

Loading performance
-------------------
Loading performance. Database contains 1564K Atoms, and 2413K edges.
CPU usage:
2:08 postgres = 82 microsecs/atom (12.2K atoms/sec)
similar to for opencog, but then AtomTable needs to reconcile, which
takes an additional 8:30 minutes to attach incoming handles!!

Conclude: database loading would be much faster if we loaded all of
the atoms first, then all of the lowest-height links, etc.  This assumes
that opencog is strictly hierarchically structured. (no "crazy loops")

After implementing height-structured restore, get following, loading
from a "hot" postgres instance (had not been stopped since previous
use, i.e. data should have been hot in RAM):
```
  cogserver = 2:36 mm:ss = 0.101 millisecs/atom (9.85K atoms/sec)
  postgres  = 1:59 mm:ss = 0.077 millisecs/atom (12.91K atoms/sec)
```
The dataset had 357162 Nodes, 1206544 Links at height 1

After a cold start, have
```
  cogserver = 2:32 mm:ss
  postgres  = 1:55 mm:ss
```
Appears that there is no performance degradation for cold-starts.

Note also: cogServer CPU usage is *identical* to its CPU usage when
loading XML! Hurrah! Also, see below: RAM usage is significantly
reduced; apparently, the reading of XML results in very bad memory
fragmentation.

Implement in-line edges, instead of storing edges in an outboard table.
```
  cogserver = 41 seconds = 26.7 microsecs/atom (37.5K atoms/sec)
  postgres  =  7 seconds =  4.56 microsecs/atom (219K atoms/sec)
```
Turn on -O3 optimization during compile ... all previous figures
were without *any* optimization. Now get
```
  cogserver = 24 seconds = 15.6 microsecs/atom (64.0K atoms/sec)
  postgres  = 11 seconds
```
Much much better!
```
10.78
23.15
```


ProtoAtoms
==========
ProtoAtoms were invented to solve the representational issues associated
with TV's. Narrowly, there is a need to store TV's of different kinds,
including TV's with counts, relative entropies, value ranges, etc.
Broadly, there is a need to store generic entity-attribute-value
information with each atom (where the atom is the entity).  ProtoAtoms
are the intended EAV solution for the atomspace.

The ProtoAtom implementation, in the main atomspace, is incomplete.
Much of the work has been done, but open design issues remain.


JSONB
=====
The storage problem for the atomspace is essentially the problem of
storing a graph, for which the EAV (Entity-Attribute-Value) pattern
seems to be the best fit.  See
https://en.wikipedia.org/wiki/Entity%E2%80%93attribute%E2%80%93value_model

A workable design point, using postgres 9.4 or newer, is JSONB. See
http://coussej.github.io/2016/01/14/Replacing-EAV-with-JSONB-in-PostgreSQL/

The goal here is to deal with protoatom storage (i.e. as a replacement
for the current TV representation problem.)

The representation of the atomspace would then look vaguely like this
(this is a rough sketch):

CREATE TABLE atomspace (
    uuid  SERIAL PRIMARY KEY,
    type  INT,    ;; the atom type.
    atom  JSONB
);

A node would be:
{
   id:   42
   type:  3   ; a ConceptNode
   atom: {
       name:    "this is a node name"
   }
}

A link would be:
{
   id:   43
   type: 4     ; an OrderedLink
   atom: {
       outgoing:  [1,2,3]
   }
}

ProtoAtom updates would look like this (e.g. for stv==SimpleTruthValue)

UPDATE atomspace
SET atom = jsonb_set(properties, '{"stv"}', '[0.2, 0.54]')
WHERE id = 42;

Critically important observations:
----------------------------------
Performance depends crucially on the use of the containment (@>)
operator in the WHERE clause. This causes the GIN index to be used,
and thus can run thousands (!) of times faster than an ordinary
EAV table structure.



TODO
====
 * See also to-do list way up top.

 * Implement the large-outgoing-set extension. A version of this can be
   found in the `postgres-dead` directory, but the code there is badly
   broken. Its probably simplest to just ignore the code in
   `postgres-dead`, and design something from scratch.

 * Store ProtoAtoms. Work is underway, see issue #513 for progress.

 * Current implementation leaks in the Values table, when the values
   are changed. This is a bug, needs fixing.

 * Consider an alternate implementation, using JSONB to do an EAV-like
   storage: For details, see
   http://coussej.github.io/2016/01/14/Replacing-EAV-with-JSONB-in-PostgreSQL/

 * Add support for multiple atomspaces.

 * Create custom table, tailored for EvaluationLink triples.
   Since majority of nodes/links in the DB will be in the form of
   EvaluationLink triples, a significant performance gain can be gotten
   by creating a custom table, and shunting queries to that.  This would
   decrease the SQL table sizes significantly, and decrease server I/O
   by factors of 2x-3x.  Another table, designed just for simple pairs,
   might help a lot, too.
