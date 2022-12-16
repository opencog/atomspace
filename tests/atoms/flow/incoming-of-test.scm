;
; incoming-of-test.scm -- Verify that IncomingOfLink works.
;

(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))

(opencog-test-runner)
(define tname "incoming-of-test")
(test-begin tname)

(define ea (Evaluation (Predicate "foo") (Concept "bar")))
(define eb (Evaluation (Predicate "foo") (Concept "zub")))
(define lf (List (Predicate "foo") (Concept "boing")))

(define inc-foo (IncomingOf (Predicate "foo")))

(test-assert "all-inc"
	(equal? (cog-arity (cog-execute! inc-foo)) 3))

(test-end tname)

(opencog-test-end)
