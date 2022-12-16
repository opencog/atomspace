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

; We expect all four above.
(test-assert "all-inc"
	(equal? (cog-arity (cog-execute! inc-foo)) 4))

(define inc-evl (IncomingOf (Predicate "foo") (Type 'EvaluationLink)))

; We expect only two.
(test-assert "inc-evl"
	(equal? (cog-arity (cog-execute! inc-evl)) 2))

; Play a game -- convert it to a set.
(define super-set (cog-execute! (CollectionOf inc-foo)))
(test-assert "inc-super"
	(equal? super-set (Set ea eb lf inc-foo inc-evl)))

(test-end tname)

(opencog-test-end)
