SQL Persist
===========
+ Linas Vepstas <linasvepstas@gmail.com>
+ Basic implementation Feb-June 2008
+ Status update May 2009
+ Status update Dec 2013
+ Status update Jan 2017

An implementation of atom persistence in SQL.  This allows not only
saving and restoring of the atomspace, but it also allows multiple
atomspaces to connect to the same SQL database at the same time, and
so share a common set of data.  That is, it implements a basic form of
a distributed atomspace.

Status
======
It works and has been used with databases containing up to 100 million
atoms (at about 1.5KByte/atom, this requires 150GBytes RAM). It has been
accessed by atomspace processes (cogservers) that ran for months to
perform computations, modifying more than 100's of millions of atoms,
without crashes or other overt trouble.  It has scaled, trouble-free,
without any slowdown, up to four cogservers.  No one has tried anything
larger than that, yet.

Unfortunately, its slow: typical save and restore speeds are in the
general ballpark of 1K Atoms/sec. This can be speeded up almost ten-fold,
by disabling various safety checks (such as sync-to-disk) in Postgres.
This works great, unless you loose power, in which case your database
will be permanently corrupted. Power loss happens more often than you
might imagine.

Features
--------
 * Save and restore of individual atoms and values.
 * Bulk save-and-restore of entire AtomSpace contents.
 * Generic API, useful for inter-server communications.

Missing features/ToDo items
---------------------------
 * Add incoming-set caching support.  Issue #1373
 * Provide optimized table layout for EvaluationLinks.
 * Provide optimized layout for sheaf sections.
 * Add support for multiple atom spaces.
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

In June 2018, it took 3500 seconds wall-clock time to load 25924129
atoms, for a rate of 7407 atoms/sec. Of these atoms, approximately half
had non-trivial values associated with them. The cogserver process used
38GB of RAM, for about 1.46KBytes/atom.

Storing portions of this dataset proceeds at about 1K atoms/sec. This
is for updating the values on the atoms, only; no actual atoms are
stored (i.e. the atoms are already in the database; as are the values;
the values are being updated).  Precisely, there were 6239904 stores
in 6561 seconds, wall-clock time, for a rate of 951 Atoms/second.

Store is performed by multiple parallel threads in the backend, each
taking to a distinct instance of postgres; thus, it appears that
postgres is the primary bottleneck. Certainly, postgres seems to be the
primary consumer of CPU time, using a combined 2x more CPU than the
atomspace. i.e. for every cpu-sec burned by the atomspace, the six
different postgres instance burn up two cpu-secs.

It is not at all obvious how to improve either load or store performance.

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

4) Provide a reference implementation for save and restore semantics.
   When saving or fetching outgoing sets, there are several choices
   of how to handle the associated values: these can also be saved
   or fetched, clobbering the atomspace contents, or some more
   fine-grained control can be provided.  The choices have both
   usability and performance implications. The choices are discussed
   in a separate section, below.

5) Discover the most minimal, simplest backing-store API. This API is
   the API between the AtomSpace, and the persistence backend.  The
   reason for keeping it as simple as possible is to minimize the work
   needed to create other backends, as well as all the standard software
   development reasons: lower complexity means fewer bugs and better
   performance.  Lower complexity means the code is easier to understand
   and use correctly.

6) A non-design-goal (at this time) is to build a system that can scale
   to more than 100 cogserver instances.  The current design might be
   able to scale to this many, but probably not much more.  Scaling
   larger than this would probably require a fundamental redesign of
   all of opencog, starting with the atomspace.

7) A non-design-goal is fully automatic save-restore of atoms.  The
   save and restore of atoms are performed under the explicit control
   by user-written code, invoking the save/restore API. There is no
   automation. Control is in the user's hands.

   The reason for this design point is that the atomspace is at too low
   a level to be fully automatic.  It cannot guess what the user really
   wants to do.  If it did try to guess, it would probably guess wrong:
   saving atoms before the user is done with them, saving atoms that get
   deleted microseconds later, fetching atoms that the user is completely
   disinterested in, clogging up RAM and wasting CPU time.  The policy
   for what to save, and when, needs to be left in control of the user
   algorithms.

   This layer only provides a mechanism.  It would be very very wrong to
   implement an automatic policy at this layer.

8) A fundamental non-goal is to provide any sort of generic object
   persistence.  The code here is meant only to save and restore atoms
   and values, and not generic C++ objects. The design of the AtomSpace
   has been carefully crafted to provide two classes of objects: the
   immutable, globally unique atoms, and the mutable valuations
   associated to atoms.  This backend mirrors this functional split.


Current Design
==============
The core design defines only a few very simple SQL tables, and some
readers and writers to save and restore atoms from an SQL database.

The current design can save/restore individual atoms, and it can
bulk-save/bulk-restore the entire contents of the AtomSpace. The above
listed goals seem to be met, more or less.

Features
--------
 * The AtomStorage class is thread-safe, and multi-threaded use is
tested in several unit tests.

 * Fully automated mapping of in-RAM atoms to in-storage universal
unique identifiers (UUID's), using the TLB mechanism.

 * Multi-user issuance and management of UUID's is weakly tested and
seems to work. This allows a form of a "distributed atomspace" --
multiple atomsapces can connect to the same database at the same time,
and save and restore atoms, getting back the correct Values (such as
TruthValues) on each atom, as expected.

 * This implementation automatically handles clashing atom types.  That
is, if the data is written with one set of atom types, and then the
atomspace process is stopped, the atomtypes are all changed (with some
added, some deleted), then during the load of the old data, the types
will be automatically translated to use the new atom types. (The deleted
atom types will not be removed from the database.  Restoring atoms with
deleted atomtypes will cause an exception to be thrown.)

 * Non-blocking atom store requests are implemented.  Eight asynchronous
write-back threads are used, with hi/lo watermarks for queue management.
That is, if a user asks that an atom be stored, then the atom will be
queued for storage, and one of these threads will perform the actual
store at a later time. Meanwhile, the store request returns to the user
immediately, so that the user can continue without blocking.  The only
time that an atom store request blocks is if the queue is completely
full; in that case, the user will be blocked until the queue drains
below the low watermark. The number of connections can be raised by
editing the AtomStorage constructor, and recompiling.

This fire-n-forget queue management algo is implemented in the C++
`AtomStorage::storeAtom()` method.  If the backlog of unwritten atoms
gets too large, the storeAtom() method may stall. Its currently designed
to stall if there's a backlog of 100 or more unwritten atoms.  This can
be changed by searching for `HIGH_WATER_MARK`, changing it and recompiling.

Queue performance statistics can be printed with the `(sql-stats)`
scheme command.

 * Reading always blocks: if the user asks for an atom, the call will
not return to the user until the atom is available.  At this time,
pre-fetch has not been implemented.  But that's because pre-fetch is
easy: the user can do it in their own thread :-)


Semantics
=========
Exactly what to save, when saving and restoring atoms, is not entirely
obvious.  The alternatives, and their implications, are discussed below.

* Saving a single node. Should all associated values be saved? Should
the user get to pick which values get saved?  Its possible that the user
only wants to save one particular value, only, so as not to clobber
other values already in the database.  The current default is to save
all associated values, when storing a single node.  This is only weakly
unit-tested; the tests are not thorough, and do not check all possible
permutations.

* Saving a single link. When a link is saved, the outgoing set of the
link must also be saved. Thus, the above considerations for node-values
also apply to the outgoing set of the link, and so on, recursively, for
the nested links.  The current default is to save all associated values,
but only on the link itself, and NOT on the entire outgoing set, when
storing a single link.  This allows granular control on the part of the
user. This is only weakly unit-tested.

* Restoring a single node or link. The above considerations for saving
run in the opposite direction, when restoring. Thus, for example, when
restoring a single node, should all associated values in the AtomSpace
be clobbered, or not?

Currently, when an atom is restored, all of the associated values are
pulled from the database, and placed in the AtomSpace, clobbering the
previous AtomSpace contents.  For links, only the values on the specified
link are fetched, and none of the others.  This is tested, but only
weakly and incompletely, in the unit tests.

* Restoring by atom type; restoring incoming sets.  Groups of atoms
can be fetched from the database: in the first case, all atoms of a
given type; in the second case, all atoms in the incoming set of a
given atom.  There are four possibilities here: (a) fetch only the
atoms, but not any of the associated values. (b) fetch the atoms
and the associated values, but not the values in the recursive outgoing
sets. (c) fetch the atoms and values, and all atoms and values,
recursively, in their outgoing set. (d) fetch the atoms, but update
the values only if they are atoms are new to the atomspace; i.e. do
not clobber existing values in the atomspace.

Currently, option (b) is implemented, and is weakly unit-tested.
It is plausible that some users may want options (a), (c) or (d).
Note that option (d) has several variations.

In the SQL backend, option (b) mostly minimizes the network and database
traffic.  For other kinds of backends, it might be more efficient to
implement option (c), and just get all the data in one big gulp.

* Restoring by pattern. This is not implemented, not done.  However,
one can imagine a situation where a pattern-matcher-like interface
is provided for the backend, so that only certain values, on certain
atoms, in certain locations in a given pattern, are fetched.

This is not done because the pattern matcher is really quite complex,
and it seems kind-of crazy to try to put this in the backend.  There
currently aren't any plausible scenarios, and plausible algorithms,
that would need this capability.



Install, Setup and Usage HOWTO
==============================
There are many steps needed to install and use this. Sorry!

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
Optionally, download and install UnixODBC devel packages.  Do NOT use
IODBC, it fails to support UTF-8!  It's also buggy when more than a few
100K atoms need to be fetched.

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

Deprecated; Optional: ODBC Device Driver Setup
----------------------------------------------
If you want to use ODBC, then you need to configure the ODBC driver.
Again: the use of ODBC is optional and discouraged; its slower clunkier
and more complex. Skip this section unless you absolutely need to use
ODBC.

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
The Postgres default configuration can be (and should be) tweaked for
performance.  The performance will be disastrously slow if the database
is not tuned.  The primary performance bottleneck is the default of
synchronous commits during writing. On spinning disk drives, this can
lead to delays of tens of milliseconds to write handfuls of atoms, as
that is the seek time (latency) for spinning disk media. Thus,
synchronous commits should be disabled.

Solid state disks are a lot faster; it's not clear if this would still
be a bottleneck.

Disabling synchronous commits may cause the latest database updates
to be lost, if power goes out, or the system unexpectedly reboots.
This kind of loss is usually not a problem for most opencog apps,
... all of the current apps are gathering statistics from generated
data, and so this kind of loss is almost surely inconsequential.

Edit `postgresql.conf` (a typical location is
`/etc/postgresql/9.3/main/postgresql.conf`) and make the changes below.
The first two changes are recommended by
http://wiki.postgresql.org/wiki/Tuning_Your_PostgreSQL_Server
```
   shared_buffers = default was 32MB, change to 25% of installed RAM
   work_mem = default was 1MB change to 32MB
   effective_cache_size = default was 128MB, change to 50%-75% of installed RAM
   synchronous_commit = default on change to off
   wal_buffers = default 64kB change to 2MB or even 32MB
   checkpoint_segments = 64 (each one takes up 16MB disk space)
	max_connections = 130 (each opencog instance needs 32)
	max_worker_processes = 32 (one per CPU core)
   ssl = off
```

For spinning media, `synchronous_commit=off` is 120x faster than
`synchronous_commit=local`(about 400 atoms/sec vs 3 atoms/sec)

For write-mostly databases, such as in the language-learning project,
you will get better results with `checkpoint_segments = 100`.

If you have postgres 9.0 or newer, there are no checkpoint_segments.
Instead, do this:
```
checkpoint_timeout = 1h
max_wal_size = 8GB
checkpoint_completion_target = 0.9
```
This will avoid the "checkpoints are occurring too frequently"
warning message.

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

For SSD drives, the following might help:
```
seq_page_cost = 0.1
random_page_cost = 0.15
effective_io_concurrency = 5
```

Unsafe performance tweaks
-------------------------
There is one very unsafe performance optimization: disabling fsync.
It's not clear how much this helps; you'd have to measure. Disabling
it is very dangerous: in case of a power loss, or a spontaneous reboot,
before all data has been written to disk, this can result in database
corruption.  Yes, this happens. THE BELOW IS NOT RECOMMENDED!

```
   fsync = default on  change to off
```

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

Verify that worked out by typing `\dg` to see:

```
                             List of roles
 Role name |                   Attributes                   | Member of
-----------+------------------------------------------------+-----------
 alex      | Superuser                                      | {}
 postgres  | Superuser, Create role, Create DB, Replication | {}
```

Then do Ctrl+D to exit, ignoring any message about `psql_history`, and
return to your own account:

```
   $ exit
```

If you ran into the error above, you still need to go back and create
the database:

```
   $ createdb mycogdata
```
There is no output if the command is successful.

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
as you use the postgres URI format.  The ODBC logins require clear-text
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
   $ cat opencog/persist/sql/multi-driver/atom.sql | psql mycogdata
```

If you are using a different user-id than your login, then you will
Have to add the `-U opencog_user` flag to the `psql` command.  If you
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
   Database          = mycogdata
   Servername        = localhost
   Port              = 5432
   Username          = opencog_user
   Password          = cheese
```

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
   The entry should look like this:
```
[opencog_test]
Description = Unit-Test DB for Opencog unit tests.
Driver      = PostgreSQL
Database    = opencog_test
Servername  = localhost
Port        = 5432
Username    = opencog_tester
Password    = cheese
```

 * The file `lib/opencog-test.conf` already has the above as the default
   username and database names.  Stick to these.


After the above steps, `BasicSaveUTest`, `PersistUTest`,
`MultiPersistUTest`,`FetchUTest` and `ValueSaveUTest` should run and
pass.


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
Bulk save and restore of atoms can be done from the guile REPL prompt
by issuing the command:
```
scheme@(guile-user)> (sql-open "postgres://...")
scheme@(guile-user)> (sql-store)
```
At this point, a progress indicator will begin printing on `stdout`;
It  will say something like this:
```
Stored 236000 atoms.
```
When finished, you will typically want to say either:
```
scheme@(guile-user)> (barrier)
```
or
```
scheme@(guile-user)> (sql-close)
```
Either of the above will stall until all atoms have been written to disk.
The `barrier` fence does not close the connection to the database, whereas
the `sql-close` does.  After a close, it is safe to stop the atomspace,
if desired; or one can continue working.  After a restart, load the data
with:
```
scheme@(guile-user)> (sql-open "postgres://...")
scheme@(guile-user)> (sql-close)
```
The completion message will be printed on `stdout`.  For example:
```
    Finished loading 973300 atoms in total
```

Individual-atom save and restore
--------------------------------
Individual atoms can be saved and fetched, using the guile interface.

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

Statistics
----------
Assorted technical statistics regarding the operation of the SQL backend
can be printed with the `(sql-stats)` command.  The accumulated
statistics can be zeroed with the `(sql-clear-stats)` command.

TLB Caching
-----------
Atoms in the database are identified with universally unique identifiers
(UUID's). The database driver contains a cache that maps UUID's to the
atoms in atomspace.  Under certain extreme circumstances, it can be
useful to clear this cache. This can be done with the
`(sql-clear-cache)` command.


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

Note also: cogserver CPU usage is *identical* to its CPU usage when
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


JSONB
=====
The storage problem for the atomspace is essentially the problem of
storing a graph, for which the EAV (Entity-Attribute-Value) pattern
seems to be the best fit.  See
https://en.wikipedia.org/wiki/Entity%E2%80%93attribute%E2%80%93value_model

A workable design point, using postgres 9.4 or newer, is JSONB. See
http://coussej.github.io/2016/01/14/Replacing-EAV-with-JSONB-in-PostgreSQL/

The goal here is to deal with how to store Values (ProtoAtoms).

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

 * Implement incoming-set caching. When an atomspace is too large to
   fit into RAM, we must leave some of it on disk.  However, it can
   become important to make sure that the incoming set of an atom has
   been loaded into RAM i.e. into the atomspace. Currently, repeatedly
   loading the incoming set is very slow and wasteful, and so its
   important to know whether an incoming set has already been loaded
   or not.  Technically, this caching can be done in the atomspace, I
   guess... See https://github.com/opencog/atomspace/issues/1373

 * Implement the large-outgoing-set extension. One way of doing this
   is touched in in https://github.com/opencog/atomspace/issues/1763
   The idea is this: Create a new link-type `ConsLink` or `ExtendLink`.
   A link with more than 330 atoms in the outgoing set would be split
   into several parts: the original link, with less than 330 atoms,
   the last atom of which would be the `ExtendLink`, which holds the
   next 330, etc. until they are all specified. This can be done with
   ZERO changes to the SQL table format.  Its really pretty easy to
   implement: just look at the length during save and restore, and
   trigger disassemble/reassembly code when the lenght limits are hit.
   Note also: the new link-type should be used in the SQL backend only,
   and thus, it would not be a real link-type, but a pseudo-type, a
   marker used only in the SQL tables.

 * Consider an alternate implementation, using JSONB to do an EAV-like
   storage: For details, see
   http://coussej.github.io/2016/01/14/Replacing-EAV-with-JSONB-in-PostgreSQL/

 * Extend the typecodes table to recording the type-inheritance
   hierarchy, so that types can be fully saved and restored from the
   database. Not clear how to resolve conflicts, if they occur, between
   the type inheritance hierarchy defined in C++, and the hierarchy
   defined in the database.  So maybe this is a *bad idea*.

 * Extend the standard table to hold count truth-values. Since almost
   all atoms have count TV's on them, this can lead to a more compact
   database format, and also could improve load and store performance,
   by reducing access to the valuations table.

 * Create custom table, tailored for EvaluationLink triples.
   Since majority of nodes/links in the DB will be in the form of
   EvaluationLink triples, a significant performance gain can be gotten
   by creating a custom table, and shunting queries to that.  This would
   decrease the SQL table sizes significantly, and decrease server I/O
   by factors of 2x-3x.  Another table, designed just for simple pairs,
   might help a lot, too.  This is easier said than done, however.

 * Add support for multiple atomspaces.

 * Create an API to allow the user to save only selected values on an
   atom.

 * Create an API to allow the user to NOT perform the recursive save of
   all values in the outgoing set (of a link).

 * Create an API to allow the user to restore only selected values on an
   atom.

 * Create an API to allow the user to NOT perform the recursive restore
   of all values in the outgoing set (of a link).

 * Create an API that provides fine-grained control over what values
   are fetched, when fetching atoms by type, or by incoming set.  See
   the section entitles "Semantics", above, for the various different
   options.
