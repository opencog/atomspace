;
; value-of-test.scm -- Unit test for FetchValueOf
;
(use-modules (opencog) (opencog persist) (opencog persist-sql))
(use-modules (opencog exec))
(use-modules (opencog test-runner))

; ---------------------------------------------------------------------
(opencog-test-runner)
(define tname "fetch-value-of")
(test-begin tname)

; Could be anything .. rocks, etc. but we just use Postgres for now.
(define sto (PostgresStorageNode "postgres:///opencog_test"))
(cog-open sto)

(define fvof (FetchValueOf (Concept "a") (Predicate "foo") sto))
(test-assert "No val" #f (cog-execute! fvof))

(test-end tname)

(opencog-test-end)
