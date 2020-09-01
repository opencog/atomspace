;
; persist-multi.scm -- Moving Atoms between multiple databases.
;
; The `persistance.scm` demo shows how to save and restore atoms to
; one database (or remote server). This demo shows how to work with
; multiple databases/servers at the same time.
;
: This demo will copy atoms between the Postgres backend and the
; RocksDB backend, although any set of backends can be used. This
; demo requires Postgres to be configured, and the RocksDB backend
; to be installed. If you want to skip Postgres, then just try the
; demo wth two different RocksDB stores.
;

(use-modules (ice-9 readline))
(activate-readline)

(use-modules (opencog) (opencog persist))

; For postgres, load the postgres module:
(use-modules (opencog persist-sql))

; For RocksDB, the rocks module. (Won't work, if not installed!)
(use-modules (opencog persist-rocks))

; For the cogserver... (this is also an external component)
(use-modules (opencog persist-cog))

; For postgres, use the test database credentials. These are the
; credentials thatthe unit tests use. Next time the unit tests run,
; they will wipe out this data, and so you should probably create
; and use your own private login.
(define psn (PostgresStorageNode "postgres://opencog_tester:cheese@localhost/opencog_test"))

; The RocksDB requires no configuration. Just use it.
(define rsn (RocksStorageNode "rocks:///tmp/atomspace-rocks-demo"))

; The cogserver requires no configuration. Just use it.
(define csn (CogserverStorageNode "cog://localhost:17001"))

; Open postgres, store one atom, and close it.
; Since only one backend is open, it will be used as the default
(cog-open psn)
(store-atom (Concept "asdf" (stv 0.318309886 0.36787944)))
(cog-close psn)

; Delete this atom.
(cog-extract! (Concept "asdf"))

; Verify it's actually gone.
(cog-prt-atomspace)

; Open all of them.
(cog-open psn)
(cog-open rsn)
(cog-open csn)

; Load everything from Postgres (Attention: this might load garbage
; from the unit tests, since we're using the unit-test db for this
; demo.)
(load-atomspace psn)

; Store everything to Rocks
(store-atomspace rsn)

; Close all of them.
(cog-close psn)
(cog-close rsn)
(cog-close csn)

; Delete this atom (again).
(cog-extract! (Concept "asdf"))
(cog-prt-atomspace)

; Open just Rocks, load everything, and take a look.
(cog-open rsn)
(load-atomspace)
(cog-close rsn)
(cog-prt-atomspace)

; And now for some fun. Put "asdf" into rocks, but with a different TV.
(cog-set-tv! (Concept "asdf") (stv 0.25 0.75))
(cog-open rsn)
(store-atom (Concept "asdf"))
(cog-close rsn)

; Open both.
(cog-open psn)
(cog-open rsn)

(fetch-atom (Concept "asdf") psn)
(fetch-atom (Concept "asdf") rsn)

(cog-close psn)
(cog-close rsn)

; That's all for now.
