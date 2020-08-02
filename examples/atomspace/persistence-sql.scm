;
; persistence-sql.scm -- Putting the AtomSpace in a "real" database.
;
; The AtomSpace is an in-RAM database. You just might want to sometimes
; write some of it out to disk, and save it for later.  The most robust
; way to do this is to attach to a commonly available, industry standard
; database. They're great for managing data, and distributing it across
; clusters, and doing cloud-type things. The AtomSpace does not try to
; reinvent this wheel.
;
; The AtomSpace has been designed with a generic "backend" layer, so
; that data can be saved to any database. A number of these have been
; tried. The one that currently works the best is PostgreSQL.
;
; -------------------------------------------------------------------
; Architectural Notes & Commentary.
;
; Some other database have been tried; this has not worked out so well.
; It turns out that the cost of converting Atoms and Values to the
; native database format (serialization/deserialization) takes up far
; more CPU time than the AtomSpace does. Thus, converting Atoms/Values
; to other formats is a (very?) costly proposition. This was
; particularly true for Neo4J.  It was tried, it didn't work out.
;
; Another issue is that most databases provide many, many features that
; are simply not needed by the AtomSpace. For example, data analytics
; is more-or-less totally useless.
;
; Another issue is that many databases compete for RAM with the
; AtomSpace; since the AtomSpace is an in-RAM database itself, having
; something else competing with it for RAM is wasteful and prevents
; larger datasets from being loadable.
;
; That said, there are then two issues worth considering:
;
; (1) Persistence-to-disk. This would probably be best achieved by
;     a simple, small, fast, single-user (key-value) database. This
;     allows datasets to be distributed as files.
;
; (2) Network communications. The best current system for this is the
;     cogserver-bases client/server system. It allows a number of
;     AtomSpaces to talk directly to one-another, in a more-or-less
;     almost peer-to-peer fashion. See the repo at
;     https://github.com/opencog/atomspace-cog and the examples there.
;
; If you still want to have a "real" database, even after reading the
; above, then the most promising is probably Apache Ignite; that's
; mostly because it has an impressive set of features, and it seems
; to play nice with C++ code. Anyway, that backend does not exist yet.
;
; ----------------------------------------
; This Demo.
;
; This file demos using the generic API to the backend. It explores
; database login, logout and atom loading and storage.  It assumes that
; PostgreSQL has been configured to run with the AtomSpace.
; Unfortunately, this can be quite challenging. Sorry!
; [The instructions are here](../../opencog/persist/sql/README.md)
;
; Note: Most of this demo will *also* work with the cogserver-based
; backend. Just cut out the SQL parts below and replace then with the
; cogserver URL.
;
; You should make sure that the unit tests pass: if you misconfigured
; the database, the unit tests will fail! Caveat Emptor!

(use-modules (ice-9 readline))
(activate-readline)

(use-modules (opencog) (opencog persist) (opencog persist-sql))

; Lets hop right in. The below should throw an exception, since
; the database is not yet open.
(store-atom (Concept "asdf" (stv 0.42 0.24)))

; Use the test database credentials. These are the credentials that
; the unit tests use. Next time the unit tests run, they will wipe
; out this data, and so you should probably create and use your own
; private login.
(sql-open "postgres://opencog_tester:cheese@localhost/opencog_test")

; Try storing again.
(store-atom (Concept "asdf" (stv 0.318309886 0.36787944)))

; Close the database.
(sql-close)

; Try fetching the atom. The database is closed -- this should fail!
(fetch-atom (Concept "asdf"))

; Reopen the database.
(sql-open "postgres://opencog_tester:cheese@localhost/opencog_test")

; Try fetching the atom. This time it should work.  Notice that
; it retrieved the correct TruthValue.
(fetch-atom (Concept "asdf"))

; One can save generic Values, as well.
(cog-set-value!
	(Concept "asdf")
	(Predicate "my key")
	(StringValue "Humpty" "Dumpty"))

(store-atom (Concept "asdf"))
(sql-close)

; The database is closed. Let's mess with the truth value.
(cog-set-tv! (Concept "asdf") (stv 0.25 0.75))

; Let's wipe out the value as well.
(cog-set-value!
	(Concept "asdf")
	(Predicate "my key")
	(StringValue "sat" "on" "a" "wall"))

(sql-open "postgres://opencog_tester:cheese@localhost/opencog_test")

(fetch-atom (Concept "asdf"))

; Look at all the keys attached to the atom:
(cog-keys (Concept "asdf"))

; Make sure the the current values are those restored from the database:
(cog-value (Concept "asdf") (Predicate "my key"))

; Other useful commands are:
;
; * `sql-load` and `sql-store` for bulk fetch and restore. For large
;   datasets, these can be slow. Extremely large datasets might not fit
;   in RAM, which is why `fetch-atom` is so handy!
;
; * `fetch-incoming-set` and `fetch-incoming-by-type` are extremely
;   useful for fetching all graphs that an atom belongs to. These
;   two are possibly the single most-important persistence calls in
;   the system. They really make the whole idea usable and easy-to-use.
;
; * `sql-stats` `sql-clear-stats` and `sql-clear-cache` print cryptic
;   performance data.
;
; That's all for now.
