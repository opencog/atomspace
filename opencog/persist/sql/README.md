SQL Persist
===========
An implementation of atom persistence in SQL.  This allows not only
saving and restoring of the AtomSpace, but it also allows multiple
AtomSpaces to connect to the same SQL database at the same time, and
so share a common set of data.  That is, it implements a basic form of
a distributed AtomSpace.

Status
======
It works and has been used with databases containing up to 100 million
atoms (at about 1.5KByte/atom, this requires 150GBytes RAM). It has been
accessed by AtomSpace processes (cogservers) that ran for months to
perform computations, modifying more than 100's of millions of atoms,
without crashes or other overt trouble.  It has scaled, trouble-free,
without any slowdown, up to four cogservers.  No one has tried anything
larger than that, yet.

Unfortunately, its slow: typical save and restore speeds are in the
general ballpark of 4K Atoms/sec. This can be speeded up almost five-fold,
by tuning the Postgres server, using SSD disks, and using HugePages.
Tuning details are given below. The [performance diary](README-perf.md)
provides some benchmarking results.

Meta-Status
-----------
In retrospect, client-server-backed storage is a fundamental design
mistake.  Why?

* A vast number of cycles are wasted serializing and unserializing
  Atoms and Values, and converting them to "unnatural" formats.
* Almost all database features, bells and whistles are not required
  and are not used.
* There are easier and faster ways to get distributed AtomSpaces, e.g.
  by using https://github.com/opencog/atomspace-cog/ which is *fast*.

In retrospect, what was really needed was a fast, efficient single-user
file-storage system, something that is tuned to the vagaries of
file-system performance. This could be provided by a fairly minimalist
key-value of "column store" database that is small, simple, fast and
feature-poor. Something that doesn't eat RAM, so that it does not
compete with the AtomSpace for RAM.

Exactly this was done with RocksDB; the code is in
https://github.com/opencog/atomspace-rocks and its 5x faster than this
Postgres backend.

Mapping the AtomSpace to RocksDB was actually quite easy, and the data
structures are simple. In retrospect, the Postgres mapping now feels
over-engineered and badly designed. It's possible (likely even?) that if
a new SQL driver was written, using the atomspace-rocks mapping, that it
would be just as fast.

In a futuristic world, there would be one nice feature to have: pattern
matching in the file-backed database. But this would have to somehow be
faster and more effective that pattern-matching in RAM, and so this is a
very high bar to exceed.


Features
--------
 * Save and restore of individual atoms and values.
 * Save and restore of atoms-by-type.
 * Save and restore of (recursive) incoming sets (by-type).
 * Bulk save-and-restore of entire AtomSpace contents.

Missing features/ToDo items
---------------------------
 * Add incoming-set caching support.  Issue #1373
 * Provide optimized table layout for EvaluationLinks.
 * Provide optimized layout for sheaf sections.
 * Add support for multiple atom spaces.

A general review of the design, the design goals and the operational
philosophy is given in the [Design README](README-design.md).


Install, Setup and Usage HOWTO
==============================
There are many steps needed to install and use this. Sorry!
Users of ODBC need to also consult the [ODBC README](README-odbc.md).

Compiling
---------
Download and install the `libpq-dev` packages, which provide the
C-language bindings to the Postgres client library.

```
  sudo apt-get install libpq-dev
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
Download and install Postgres version 9.5 or newer.  The current design
simply won't work with MySQL, because of a lack of array support.
Same holds true for SQLite.  Sorry. There is some work in the
code-base to support these other databases, but the work-arounds for
the missing features are kind-of complicated, and likely to be slow.
(The above statements were accurate in 2012; things may have changed.)

Be sure to install the Postgres server and the Postgres client.

```
    sudo apt-get install postgresql
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

The difference between `synchronous_commit=off` and `=on` can be as
much as a factor of 100x for spinning disks, and a factor of 5x for
SSD drives, based on measurements on actual AtomSpace workloads.

Edit `postgresql.conf` (usually `/etc/postgresql/9.6/main/postgresql.conf`)
and make the changes below.  The first two changes are recommended by the
[PostgreSQL wiki](http://wiki.postgresql.org/wiki/Tuning_Your_PostgreSQL_Server)
```
   shared_buffers = 24GB       # Change to 25% of installed RAM
   huge_pages = try            # Always use huge_pages, if possible.
   work_mem = 32MB             # Default was 1MB, change to 32MB
   effective_cache_size = 60GB # Change to 25%-75% of installed RAM
   synchronous_commit = off    # Default was on, change to off
   max_connections = 135       # Each AtomSpace instance needs 32
   max_worker_processes = 32   # One per CPU core
   ssl = off                   # There's no point to encryption locally
```

Avoid the "checkpoints are occurring too frequently" warning message by
setting:
```
   checkpoint_timeout = 1h
   max_wal_size = 8GB
   checkpoint_completion_target = 0.9
```

For SSD drives, the following can make a significant difference.
This is shown in some [benchmark charts at a blog](https://portavita.github.io/2019-07-19-PostgreSQL_effective_io_concurrency_benchmarked/).
The changes to `seq_page_cost` and `random_page_cost` represent the
relative ratios of SSD speed to CPU/RAM speed (where `1.0` is for a
spinning disk.)
```
  seq_page_cost = 0.1
  random_page_cost = 0.1
  effective_io_concurrency = 100
```

Restarting the server might lead to errors stating that max shared mem
usage has been exceeded. This can be fixed by telling the kernel to use
42.4 gigabytes (for example). Edit: `sudo vi /etc/sysctl.conf` and add:
```
   kernel.shmmax = 42440100100
```
Save file contents, then:
```
   sudo sysctl -p /etc/sysctl.conf
```

#### Tuning HugePages
Using 2MB-sized HugePages can also offer a large performance boost,
both for Postgres, and for the atomspace, especially when working with a
large atomspace.  The procedure to set these up is a bit complicated.
First, add a hugepages user-group, and add postgres to it:
```
   sudo groupadd hugepages
   sudo gpasswd -a postgres hugepages
```
Then you need to find out what the group id (gid) was:
```
   id postgres
```
Suppose that this shows group id 1234 for `hugepages`.  This needs to
be added to `/etc/sysctl.conf` as well. So edit, and add:
```
   vm.nr_hugepages = 16384       # 32GB of hugepages, 25% of RAM.
   vm.hugetlb_shm_group=1234
```
(You might even want to set hugepages to 80% of RAM, or whatever your
typical working-set size is for your typical AtomSpace.  For server-class
systems, you might also want to experiment with 1GB hugepages).

Don't forget to `sudo sysctl -p /etc/sysctl.conf` again.

Finally, the ability to use those pages. Add to `/etc/security/limits.conf`:
```
    @hugepages      soft    memlock         unlimited
    @hugepages      hard    memlock         unlimited
```

Database Setup
--------------
A database to hold the OpenCog data needs to be created.  Multiple
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
Then:
```
   $ createdb alex # Replace this with your username
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
create the OpenCog tables.  If the above didn't work, then you will
have to create an explicit database user login, as explained below;
otherwise, creating a user is optional.


Does it work now?
-----------------
If the above steps were properly carried out, then the system is ready
to be used. A simple "sniff test" can verify this.  At the shell prompt,
start `guile`, and then try the following:
```
   $ guile
   scheme> (use-modules (opencog))
   scheme> (use-modules (opencog persist) (opencog persist-sql))
   scheme> (sql-create "postgres:///foo")
   scheme> (sql-open "postgres:///foo")
   scheme> (Concept "this is a test" (stv 0.4 0.6))
   scheme> (store-atom (Concept "this is a test"))
   scheme> (sql-stats)
   scheme> (sql-close)
```

The above should work without any errors. If not, double-check your
configuration. You can verify that the truth value was stored by
fetching the Atom; alll associated values will be fetched:

```
   $ guile
   scheme> (use-modules (opencog))
   scheme> (use-modules (opencog persist) (opencog persist-sql))
   scheme> (sql-open "postgres:///foo")
   scheme> (fetch-atom (Concept "this is a test"))
   scheme> (sql-close)
```

Other useful scheme operations include bulk load/store, such as
`store-atomspace`, `load-atomspace` and `load-atoms-of-type`.

Even more handy are operations that fetch only a small portion
of the atomspace: `fetch-incoming-set`, `fetch-incoming-by-type`,
`load-referers` and `store-referers`.

You can be reminded of these by saying
```
   scheme> ,a sql
   scheme> ,a fetch
```
and get specific documentation on each:
```
   scheme> ,d sql-open
```
which will, for example, remind you of the supported URL formats:
```
   postgres:///DBNAME
   postgres://USER@HOST/DBNAME
   postgres://USER:PASSWORD@HOST/DBNAME
   postgres:///DBNAME?user=USER
   postgres:///DBNAME?user=USER&host=HOST
   postgres:///DBNAME?user=USER&password=PASS
```

The `foo` database created above can be deleted at the shell prompt:
```
   $ dropdb foo
```

There is currently no way to drop databases from the scheme prompt;
this is partly a safety feature (fewer accidents), and partly a
minimization of complexity. If you need complex database management,
then you need a database management system. This guile module is
not a replacement for a full DBMS, nor could it ever be.

Skip ahead to the section [Using the System](#using-the-system)
below for more info.

User setup
----------
(Optional)  The above is sufficient for most simple use-cases. The unit
tests do require that a distinct user be created, called `opencog_tester`,
although this can be bypassed by altering the database credentials in the
OpenCog test configuration file, in `/lib/atomspace-test.conf`. This
section provides a quick review of postgres user management.

If no users are set up, then postgres automatically provides password-less
`peer` authentication to your unix username. This is enough for
single-user database access.  Shared dtabases require more work.

The database user is NOT the same thing as a unix user: the login is for
the database (only), not the OS. In general, you will want to pick a
password that is DIFFERENT than your unix-password. This is because some
of the database login methods require a clear-text password to be
supplied. The full set of postgres login styles are supported, as long
as you use the postgres URI format.

See the [postgres
documentation]((https://www.postgresql.org/docs/9.6/static/libpq-connect.html)
for details on the allowed URI format.

In the following, a database user named `opencog_tester` is created, and
given the password `cheese`.  You can pick a different username and
password. Do NOT use your unix password!  Pick something else! Create
the user at the shell prompt:

```
   $ psql -c "CREATE USER opencog_tester WITH PASSWORD 'cheese'" -d opencog_test
```

The above assumes you will be using a database called `opencog_test`, as
configured in `/lib/atomspace-test.conf`.  Check that the above worked,
by manually logging in:

```
   $  psql opencog_test -U opencog_tester -W -h localhost
```

If you can't login, something up above failed.

Next, see if you can login using unix-domain sockets, instead of
TCP-IP sockets to `localhost`:

```
   $  psql opencog_test -U opencog_tester
```

For most users, this will fail with the message
```
psql: FATAL:  Peer authentication failed for user "opencog_tester"
```
To fix this, you need to edit the file
```
   /etc/postgresql/9.5/main/pg_hba.conf
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

Manual table initialization
---------------------------
Next, you need to populate the database with OpenCog-specific tables.
Navigate to the atomspace folder you cloned from GitHub:

```
   $  cd ~/src/atomspace
```

Create the database tables:

```
   $ cat opencog/persist/sql/multi-driver/atom.sql | psql opencog_test
```

If you are using a different user-id than your login, then you will
Have to add the `-U opencog_tester` flag to the `psql` command.  If you
created a distinct user, and did not set up the `hba.conf` file as
above, then you also need a `-h localhost` flag, to access the database
using TCP/IP sockets on the local network.

Verify that the tables were created. Login as before:

```
   $  psql opencog_test
```

Then enter `\dt` at the postgres prompt.  You should see this:

```
    opencog_test=> \dt
                  List of relations
     Schema |    Name    | Type  |     Owner
    --------+------------+-------+----------------
     public | atoms      | table | opencog_tester
     public | spaces     | table | opencog_tester
     public | typecodes  | table | opencog_tester
     public | valuations | table | opencog_tester
     public | values     | table | opencog_tester

    (5 rows)
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


After the above steps, all nine unit tests should run and pass, starting
with `BasicSaveUTest`, `PersistUTest`, `MultiPersistUTest`, and so on.


Unit Test Status
----------------
* As of 2011-04-29 bzr revision 5314, BasicSaveUTest (still) passes.
* As of 2013-11-26 BasicSaveUTest works and passes, again.
* As of 2014-06-19 both unit tests work and pass.
* As of 2015-04-23 both unit tests work and pass.
* As of 2017-01-20 all four unit tests work and pass.
* As of 2019-02-01 all six unit tests work and pass.
* As of 2019-11-11 all nine unit tests work and pass.


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
Create a database with `sql-create`:
```
scheme@(guile-user)> (sql-create "postgres:///foo")
```
and then open it with `sql-open`:
```
scheme@(guile-user)> (sql-open "postgres:///foo")
```
There are other, alternate forms for the URI. See the [postgres
documentation](https://www.postgresql.org/docs/9.6/static/libpq-connect.html)
for details.
```
> (sql-open "odbc://atomspace_user:cheese/foo")
> (sql-open "postgres://atomspace_user@localhost/foo")
> (sql-open "postgres://atomspace_user:cheese@localhost/foo")
> (sql-open "postgres:///foo?user=atomspace_user")
> (sql-open "postgres:///foo?user=atomspace_user&host=localhost")
> (sql-open "postgres:///foo?user=atomspace_user&password=cheese")
```

Save an atom with `store-atom`:
```
scheme@(guile-user)> (define x (ConceptNode "asdfasdf" (stv 0.123 0.789)))
scheme@(guile-user)> (store-atom x)
```

Other useful scheme functions: `fetch-atom` and `fetch-incoming-set`.
A debugging print: `sql-stats`

Bulk load and restore: `sql-load` `sql-store` `sql-close`.

View the list of supported functions with the `,apropos` guile shell
command:
```
   scheme> ,a sql
   scheme> ,a fetch
```
Specific documentation cane be viewed with the `,describe` command:
```
   scheme> ,d sql-open
```

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

*** The End ***
