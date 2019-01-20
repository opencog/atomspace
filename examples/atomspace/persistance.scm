;
; persistance.scm -- Putting the AtomSpace in a "real" database.
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
; tried. The one that works the best is PostgresSQL. Some have not
; worked out very well: we tried some Java-based graph DB's, but the
; network overhead is a real killer, and they are much too slow.
;
; The most promising future backend is probably Apache Ignite; that's
; mostly because it has an impressive set of features, and it seems
; likely that it will interface well with C++ code. Anyway, that does
; not exist yet.
;
; This file demos using the generic API to the backend. It explores
; database login, logout and atom loading and storage.  It assumes that
; PostgreSQL has been configured to run with the AtomSpace.
; Unfortunately, this can be quite challenging. Sorry!
; [The instructions are here](../../opencog/persist/sql/README.md)
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

; Change it's truth value, store it, and repeat.
(define my-atom (Concept "asdf"))
(cog-set-tv! my-atom (stv 0.25 0.75))

; One can save generic Values, as well.
(define my-key (Predicate "my key"))
(cog-set-value! my-atom my-key (StringValue "Humpty" "Dumpty"))

(store-atom my-atom)
(sql-close)
(sql-open "postgres://opencog_tester:cheese@localhost/opencog_test")
(fetch-atom (Concept "asdf"))

; Look at all the keys attached to the atom:
(cog-keys my-atom)

; Make sure the values were restored:
(cog-value my-atom my-key)

; That's all for now
