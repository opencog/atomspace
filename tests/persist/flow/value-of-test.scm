;
; value-of-test.scm -- Unit test for FetchValueOf, StoreValueOf
;
(use-modules (opencog) (opencog persist) (opencog persist-sql))
(use-modules (opencog exec))
(use-modules (opencog test-runner))

; ---------------------------------------------------------------------
(opencog-test-runner)
(define tname "fetch-value-of")
(test-begin tname)

; Could be any StorageNode, e.g. RocksStorageNode, etc.
; but, for convenience, we just use Postgres for now.
(define sto (PostgresStorageNode
	"postgres:///opencog_test?user=opencog_tester&password=cheese"))
(cog-open sto)

(define fvof (FetchValueOf (Concept "a") (Predicate "foo") sto))
(test-assert "No val" (nil? (cog-execute! fvof)))

(define sv (StringValue "a" "b" "c"))
(cog-set-value! (Concept "a") (Predicate "foo") sv)
(test-assert "Unchanged" (equal? sv (cog-execute! fvof)))

(define fv (FloatValue 1 2 3))
(cog-set-value! (Concept "a") (Predicate "foo") fv)
(test-assert "Still Unchanged" (equal? fv (cog-execute! fvof)))

(define svof (StoreValueOf (Concept "a") (Predicate "foo") sto))
(test-assert "maybe-stored" (equal? fv (cog-execute! svof)))

; Set it to string, it should reset back to float.
(cog-set-value! (Concept "a") (Predicate "foo") sv)
(test-assert "Reset" (equal? fv (cog-execute! fvof)))

(cog-delete! (Concept "a"))
(cog-close sto)
(test-end tname)

(opencog-test-end)
