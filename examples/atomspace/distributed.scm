;
; distributed.scm -- Distributed AtomSpace via SQL networking.
;
; The AtomSpace is an in-RAM database, with several backends that allow
; data storage and network communications.  This particular demo shows
; the Postgres SQL backend, and how to use it to have multiple
; AtomSpaces communicate with each other over the network.
;
; The AtomSpace has a built-in "Backend API" - a small and simple layer
; for sending/receiving (or saving/restoring) Atoms and Values, either
; individually, or in groups, to whatever backend was provided. This
; demo is for the PostgreSQL backend (which is networked).
;
; Besides SQL, there is also a direct AtomSpace-to-AtomSpace network
; client/server interface at https://github.com/opencog/atomspace-cog/
; It has its own examples, although the example below also works with
; that system. (Actually, it "works better" -- the "cog" backend is
; maybe 2x or 3x faster than the Postgres backend. The reason for this
; is that Postgres pays a heavy price for data serialization: it has
; to convert from the native AtomSpace format into SQL, and then send
; the SQL over the net, then unpack the network packets at the server,
; and finally do something with them! Phew! That process uses the vast
; majority of the total CPU usage. The direct AtomSpace-to-AtomSpace
; communications avoids most of this overhead.)
;
; -------------------------------------
; Architecture
;
; Understanding the overall architecture is important. Some key ideas:
;
; * From the network point-of-view, the AtomSpace seems to be a thin
;   layer on top of a network-distribution layer.
; * The network distribution can be provided by anything for which a
;   "BackingStore API" has been created.
; * One of these is PostgreSQL, the subject of this demo.
;
; The goal of the layering is to provide a minimal API that allows
; Atoms and Values to be shipped across the network (or to a file-based
; database). Minimal API's mean:
;
; -- easier to implement
; -- minimal overhead
;
; The problem with layering is that it requires format conversions:
; the Atoms and Values have to be converted to the native format of
; the other system (i.e. have to be serialized/deserialized). This
; is surprisingly CPU-intensive! (... because Atoms/Values are really
; very small and simple, so they're fast, and just about anything else
; is slower...)
;
; In principle, one *could* create a backend for whiz-bag super-ultra
; very-popular and highly-recommended distributed database XYZ.
; This way, the distribution and scalability issues can be handled by
; system XYZ -- and *bingo*, you've got a highly distributed AtomSpace.
;
; Or ... that's the theory. In practice, the serialization/deserialization
; costs overwhelm performance. It currently appears that the best way
; to use the AtomSpace is in a "decentralized" fashion, with
; peer-to-peer communication between AtomSpaces (e.g. with the
; previously mentioned https://github.com/opencog/atomspace-cog/)
; and provide persistence with a fast, simple file-backed single-user
; key-value DB. e.g. RocksDB.
;
; (The other point is that none of the whizzy bells-n-whistles of the
; whizzy database XYZ are needed. Those whizzy data analytics tools
; are useless on AtomSpace data... why? The Atomspace stores natural
; language and logic and robot control info and computational biology
; info. The data analytics tools have no clue about any of this. They
; can't analyze it because they don't know what it means.)
;
;-------------------------------------------------------------
; Some blogs about scaling PostgreSQL:
;
; https://blog.timescale.com/scalable-postgresql-high-availability-read-scalability-streaming-replication-fb95023e2af
; https://www.enterprisedb.com/blog/horizontal-scalability-postgresql-96
; https://www.cybertec-postgresql.com/en/services/administration/postgresql-performance-and-scalability/
;
; This demo is a minor variant of the demo in `persistence.scm`. It
; uses two AtomSpaces, running on different machines, connecting to the
; same PostgreSQL backend. This assumes that you have correctly
; configured PostgreSQL for network operation. This is not easy.
; [The instructions are here](../../opencog/persist/sql/README.md)
;
; You should make sure that the unit tests pass. There are ten unit
; tests that check the PostgreSQL backend. If you misconfigured the
; database, the unit tests will fail. Passing them will get you started.
;
; -------------------------------------------------
; Demo steps:
; * Make sure that you understand how to run database operations on a
;   single node first -- see the demo example `persistence-sql.scm`.
;   This includes installing and configuring Postgresql as described
;   [here](../../opencog/persist/sql/README.md)
; * The below assumes that Postgres is running at the network address
;   10.70.70.2 -- change this as needed (and use DNS).
; * Log in (ssh) on two different machines connected by a network.
;   They are called "A" and "B" below.
; * Get to the guile command prompt on both machines.
; * Be prepared to cut-n-paste from this file to both machines.
; * Follow instructions below.
;
; Cut-n-paste following to both machines:
(use-modules (opencog) (opencog persist) (opencog persist-sql))

; Log in from both machines. This must not fail or error-out.
(sql-open "postgres://opencog_tester:cheese@10.70.70.2/opencog_test")

; On machine "A" only:
(store-atom (Concept "asdf" (stv 0.318309886 0.36787944)))

; On machine "B" only:
(fetch-atom (Concept "asdf"))

; Notice that the above obtained the correct TruthValue, specified on
; machine "A".   Both "A" and "B" now have the same Atom, having the
; same TruthValue. This is bi-directional. Try going the other
; direction.

; On machine "B" only:
(store-atom (Concept "asdf" (stv 0.99 0.66)))

; On machine "A" only:
(fetch-atom (Concept "asdf"))

; Again, notice that the TruthValue updated correctly.

; Automated distribution of other values also works.
; On machine "B", issue this:
(cog-set-value!
	(Concept "asdf")
	(Predicate "my key")
	(StringValue "Humpty" "Dumpty"))
(store-atom (Concept "asdf")) ; On machine B

; On machine "A" only:
(fetch-atom (Concept "asdf"))

; On machine "A", look at all the keys attached to the atom:
(cog-keys (Concept "asdf"))

; On machine "A", look at the value on "my key":
(cog-value (Concept "asdf") (Predicate "my key"))

; Use the above to practice sending other atoms and values between "A"
; and "B".
; ---------------------
; Sending and receiving one atom at a time is a bit tedious. There are
; various different commands to store and fetch larger subsets.

; On machine "A" only:
(Evaluation (stv 0.8 0.5)
	(Predicate "foo")
	(List (Concept "asdf") (Concept "qwerty")))

(Member (Concept "asdf") (Concept "keyboard"))

(Inheritance (Concept "asdf") (Concept "string of letters"))

; Export all of the above with one command (on machine "A" only):
(store-referers (Concept "asdf"))

; The `store-referers` command will recursively walk the entire incoming
; set of `(Concept "asdf")` and push all those atoms out to the network
; server.

; On machine "B" only:
(fetch-incoming-by-type (Concept "asdf") 'MemberLink)

; Verify that the MemberLink (and the MemberLink only) came across:
(cog-incoming-set (Concept "asdf"))

; Get all of the incoming set, and make sure it arrived:
(fetch-incoming-set (Concept "asdf"))
(cog-incoming-set (Concept "asdf"))

; Notice that only one layer-level was fetched. The ListLink does not
; have an incoming set (yet; it was not fetched):
(cog-incoming-set (List (Concept "asdf") (Concept "qwerty")))

; Now fetch the EvaluationLink as well:
(load-referers (Concept "asdf"))

; Notice that the EvaluationLink came across, together with it's
; TruthValue.
(cog-incoming-set (List (Concept "asdf") (Concept "qwerty")))

; ---------------------
; Other useful commands are:
;
; * `sql-load` and `sql-store` for bulk fetch and restore. For large
;   datasets, these can be slow. Extremely large datasets might not fit
;   in RAM, which is why `fetch-atom` is so handy!
;
; * `sql-stats` `sql-clear-stats` and `sql-clear-cache` print cryptic
;   performance data.
;
; Try this, on both machines:
(sql-stats)

; The (sql-clear-stats) just resets the stats printed above.
; The (sql-clear-cache) will reset the local cache of atoms fetched from
; the distributed network server. This is "harmless", in that the
; operation of the AtomSpace will not be affected. However, it will
; affect performance: future atom fetches will take a bit longer,
; because they have to refill the cache. The only thing that clearing
; the cache is good for is to free up some RAM.

; ---------------------
;
; That's all for now.
; The Distributed AtomSpace is a work-in-progress. The above works, it
; works really pretty well, and seems to scale just fine to reasonable
; sizes (about 200M Atoms so far).  There is no doubt that improvements
; could be made:
;
; -- Create a scatter-gather (map-reduce) type layer to simplify
;    parallel processing of large datasets.
; -- Double-check that atomic ops are working correctly (so that
;    summations and counters working across multiple machines get the
;    correct grand-total.
; -- Improve read-write overlays on read-only AtomSpaces.
; -- Whatever else might be needed, based on hands-on, practical
;    experience.
;
; The last bullet is the most important: without practical experience,
; the road ahead is hard to map out.
