;
; distributed.scm -- (Network/Cluster/Cloud) Distributed AtomSpace.
;
; The AtomSpace is an in-RAM database, backed by a network database.
; This allows the AtomSpace to run across multiple computers in a
; distributed fashion (network, cluster or cloud). This provides a
; very basic demo.
;
; Understanding the overall architecture is important. Some key ideas:
;
; * The AtomSpace is a thin layer on top of a network-distribution layer. 
; * The network distribution is provided by popular 3rd-party systems.
; * Defacto, today, this means PostgreSQL (We've tried others. See below.)
;
; The reason for this layering is that creating a scalable distributed
; system "from scratch" is hard. There are already dozens of systems
; that do this that are popular, reliable, well-known. It's more-or-less
; pointless to try to re-invent this technology; it's much easier to
; leverage existing systems. Treat this as modular architecture: use
; well-tested, debugged high-performance systems that already exist.
; Don't reinvent them.
;
; The layering is done with a generic "backend" layer. The AtomSpace
; works with the backend to save and fetch individual Atoms or various
; sets of Atoms. The backend is repsonsible for network distribution.
;
; Several different backends have been implemented. Only one is fully
; functional, complete, debugged -- the PostgreSQL backend. Others
; were attempted but abandoned because they were too slow. It might
; be nice to have a choice: for example, Apache Ignite looks very
; promising. A backend for this does not yet exist.
;
; This demos is a minor variant of the demo in `persistence.scm`. It
; uses two AtomSpaces, running on different machines, connecting to the
; same PostgreSQL backend. This assumes that you have correctly
; configured PostgreSQL for network operation. This is not easy.
; PostgreSQL has been configured to run with the AtomSpace.
; [The instructions are here](../../opencog/persist/sql/README.md)
;
; You should make sure that the unit tests pass: if you misconfigured
; the database, the unit tests will fail! Caveat Emptor!

(use-modules (ice-9 readline))
(activate-readline)

(use-modules (opencog) (opencog persist) (opencog persist-sql))

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

(store-atom my-atom)
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
; That's all for now
