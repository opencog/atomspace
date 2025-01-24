;
; persistence.scm -- Putting the AtomSpace into "normal" database.
;
; The AtomSpace is an in-RAM database. You just might want to sometimes
; write some of it out to disk, and save it for later; or maybe you want
; to share AtomSpace contents with other AtomSpaces. This is accomplished
; with "persistence" plug-in modules. These provide `StorageNodes` that
; can be opened, closed, read from and written to. As of this writing,
; there are four stable, supported modules for doing this:
;
; (persist-rocks) - Stores the AtomSpace to disk, using RocksDB.
;                   This is the easiest to use: no configuration is
;                   required. Just start it and go. Also: its fast!
; (persist-sql)   - Stores the AtomSpace to a PostgreSQL database.
;                   This can be accessed/shared by multiple users,
;                   thus allowing a form of distributed processing.
;                   Three problems:
;                    * Postgres can be hard to configure for beginners.
;                    * It's slower than the RocksDB node.
;                    * The current implementation is stale and needs
;                      a refresh.
; (persist-cog)   - Communicates with another AtomSpace via TCP/IP.
;                   This can be accessed/shared by multiple users,
;                   thus allowing a form of distributed processing.
;                   This requires configuring a CogServer to run on
;                   a second network node, and so is not quite as
;                   easy as using RocksDB.
; (persist-file)  - Load and store AtomSpace contents as scheme
;                   s-expressions. This is 10x faster than using the
;                   scheme interpreter to load Atomese data. However,
;                   this is just a flat file: once written out, there
;                   is no way to automatically edit or update it.
;                   (It's just ASCII text, you can text-edit it, of
;                   course.)
;
; All of the above use exactly the same API, the `StorageNode` API.
; This demo illustrates the RocksDB `StorageNode`; the Postgres and
; the CogServer nodes work exactly the same way.
;
; The `FileStorageNode` implements a subset of the `StorageNode` API,
; just enough to read and write plain-ASCII (plain-UTF8) flat files
; containing s-expressions.
;
; It may be useful to review the `persist-store.scm` example before
; studying this one.
;
; -------------------------------------------------------------------
; Architectural Notes & Commentary.
;
; Before we begin, there is some important background to be aware of.
;
; Besides the modules mentioned above, other SQL, no-SQL and graph
; databases have been tried. This has not worked out so well. It turns
; out that the cost of converting Atoms and Values to the native database
; format (serialization/deserialization) takes up far more CPU time than
; the AtomSpace does. That is, converting Atoms/Values into key-value
; pairs (for no-SQL databases) or into row/column format (for SQL) or
; even into vertices and edges (for graph databases) is a very costly
; proposition. It take a *lot* of CPU time! Worse, most databases are
; network-enabled, and thus have to create packets, send them and decode
; them at the other end, which adds even more overhead!  We found this
; out the hard way with Neo4J, which ran literally 1000x slower (that's
; right, one-thousand times slower) than what the AtomSpace can do,
; in-RAM. Performance is hard, it turns out.
;
; Another issue is that most databases provide many, many features that
; are simply not needed by the AtomSpace. For example, data analytics
; is more-or-less totally useless. You can store the AtomSpace in a DB,
; but the format defies data analytics, because the AtomSpace format
; encodes language, logic, reasoning, bio-science and other formats that
; are completely opaque to external database systems.
;
; Another issue is that many databases compete for RAM with the
; AtomSpace; since the AtomSpace is an in-RAM database itself, having
; something else competing with it for RAM is wasteful and prevents
; larger datasets from being loadable.
;
; There appear to be three issues to consider when designing apps that
; use the AtomSpace:
;
; (1) Persistence-to-disk. This is best achieved by using the RocksDB
;     module. Its really quite fast, and reasonably compact. It saves
;     the AtomSpace to a file, and you can use ordinary file-management
;     tools to copy distributed and backup RocksDB AtomSpaces. See
;     https://github.com/opencog/atomspace-rocks and the examples there.
;
; (2) Network communications. The best current system for this is the
;     CogServer-based client/server system. A single CogServer can scale
;     to approximately a dozen clients, all sharing data via the common
;     CogServer. This is a single hub-n-spoke model; by running many
;     hubs (many CogServers) one can have a crude distributed AtomSpace
;     system. Of course, two CogServers can talk to one-another in a
;     peer-to-peer fashion. See the repo at
;     https://github.com/opencog/atomspace-cog and the examples there.
;
; (3) Proxying and coordination of distributed work. Consider a
;     CogServer sitting on top of RocksDB. When a client goes to fetch
;     an Atom from the CogServer (that is, from the AtomSpace inside
;     the CogServer), it can happen that it's not there because its
;     still on disk, and hasn't been loaded yet. The ProxyNodes solve
;     this problem: they're configurable agents that can pass requests
;     on to others. They can be configured in arbitrarily complicated
;     ways, to pipe AtomSpace data around between various locations.
;
; ----------------------------------------
; This Demo.
;
; This file demos using the generic API to the backend. It shows some
; basics: database login, logout and atom loading and storage. The
; RocksDB node requires zero configuration to run, and thus is very easy.
;
; This demo can also run, almost unchanged, with the Posgres backend,
; or the CogServer backend.
;
; The Postrges node is considerably more difficult: It assumes that
; PostgreSQL has been configured to run with the AtomSpace.
; Unfortunately, this can be quite challenging. Sorry!
; The instructions are here
:   https://github.com/opencog/atomspace-pgres/blob/master/opencog/persist/sql/README.md
; Please make sure that the unit tests pass: if you misconfigured
; the database, the unit tests will fail! Caveat Emptor!
;
; To run the CogServer, review the documentation on both the cogserver
; and the atomspace-cog git repos.

(use-modules (ice-9 readline))
(activate-readline)

(use-modules (opencog) (opencog persist))

; Lets hop right in. The below should throw an exception, since
; no StorageNode is open yet.
(store-atom (Concept "asdf" (stv 0.42 0.24)))

; To run the demo with RocksDB, load the rocks module. This will fail,
; if the module is not installed.
(use-modules (opencog persist-rocks))

; The same, for Postgres.  If you have not installed or configured,
; then just skip this step.
(use-modules (opencog persist-sql))

; For the CogServer... skip this if you haven't set it up.
(use-modules (opencog persist-cog))

; The RocksDB requires no configuration. Just use it.
(define rsn (RocksStorageNode "rocks:///tmp/atomspace-rocks-demo"))

; For Postgres, use the test database credentials. These are the
; in the form of "user:password@example.com". The below uses the
; credentials that the unit tests use. The next time the unit tests
; run, they will wipe out this data, so don't expect it to stay there.
; For production systems, you need to create and use your own private
; login.
(define psn (PostgresStorageNode "postgres://opencog_tester:cheese@localhost/opencog_test"))

; The CogServer requires no configuration, other than starting the
; Cogserver. Just use it. The URL here is for a cogserver running on
; this host; change localhost to example.com for a remote machine.
; The 17001 is the default CogServer port number.
(define csn (CogStorageNode "cog://localhost:17001"))

; Lets use Rocks, for now.
(cog-open rsn)

; Try storing again.
(store-atom (Concept "asdf" (stv 0.318309886 0.36787944)))

; Close the database.
(cog-close rsn)

; Try fetching the atom. The database is closed -- this should fail!
(fetch-atom (Concept "asdf"))

; Reopen the database.
(cog-open rsn)

; Try fetching the atom. This time it should work.  Notice that
; it retrieved the correct TruthValue.
(fetch-atom (Concept "asdf"))

; One can save generic Values, as well.
(cog-set-value!
	(Concept "asdf")
	(Predicate "my key")
	(StringValue "Humpty" "Dumpty"))

(store-atom (Concept "asdf"))
(cog-close rsn)

; The database is closed. Let's mess with the truth value.
(cog-set-tv! (Concept "asdf") (stv 0.25 0.75))

; Let's wipe out the value as well.
(cog-set-value!
	(Concept "asdf")
	(Predicate "my key")
	(StringValue "sat" "on" "a" "wall"))

(cog-open rsn)

(fetch-atom (Concept "asdf"))

; Look at all the keys attached to the atom:
(cog-keys (Concept "asdf"))

; Make sure the the current values are those restored from the database:
(cog-value (Concept "asdf") (Predicate "my key"))

; Other useful commands are:
;
; * `load-atomspace` and `store-atomspace` for bulk fetch and restore.
;   For large datasets, these can be slow. Extremely large datasets
;   might not fit in RAM, which is why `fetch-atom` is so handy!
;
; * `fetch-incoming-set` and `fetch-incoming-by-type` are extremely
;   useful for fetching all graphs that an atom belongs to. These
;   two are possibly the single most-important storage calls in
;   the system. They really make the whole idea usable and easy-to-use.
;
; * `sql-stats` `sql-clear-stats` and `sql-clear-cache` print cryptic
;   performance data for the SQL backend. Similar commands for Rocks.
;
; * `sql-open` and `sql-close` are similar to `cog-open` and `cog-close`,
;   except they take the URL directly. Unfortunately, this means that
;   one cannot work with more than one connection at a time this way.
;   Examples:
;   (sql-open "postgres://opencog_tester:cheese@localhost/opencog_test")
;   (sql-close)
;
; That's all for now.
