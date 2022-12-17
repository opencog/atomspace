;
; signature-test.scm -- Verify that SignatureLinks work.
;

(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))

(opencog-test-runner)
(define tname "signature-test")
(test-begin tname)

(Evaluation
	(Predicate "foo")
	(List (Concept "A") (Concept "B")))
(Evaluation
	(PredicateNode "bar")
	(List (Concept "C") (Concept "D")))
(Evaluation
	(Predicate "foo")
	(List (Number 5) (Number 6)))

(define meet-b
	(Meet
		(TypedVariable (Variable "$x") (Type 'ConceptNode))
		(Evaluation
			(PredicateNode "foo")
			(List (Sign 'Concept) (Variable "$x")))))

(define meet-bd
	(Meet
		(TypedVariable (Variable "$x") (Type 'ConceptNode))
		(Evaluation
			(Sign 'PredicateNode)
			(List (Sign 'Concept) (Variable "$x")))))

(define meet-foon
	(Meet
		(TypedVariable (Variable "$p") (Type 'PredicateNode))
		(Evaluation
			(Variable "$p")
			(List (Sign 'Number) (Sign 'Number)))))

(define meet-sfoon
	(Meet
		(TypedVariable (Variable "$p") (Type 'PredicateNode))
		(Evaluation
			(Variable "$p")
			(Signature (List (Type 'Number) (Type 'Number))))))

(test-assert "got meet-b"
	(equal? (cog-execute! (CollectionOf meet-b))
		(Set (Concept "B"))))

(test-assert "got meet-bd"
	(equal? (cog-execute! (CollectionOf meet-bd))
		(Set (Concept "B") (Concept "D"))))

(test-assert "got meet-foon"
	(equal? (cog-execute! (CollectionOf meet-foon))
		(Set (Predicate "foo"))))

(test-assert "got meet-sfoon"
	(equal? (cog-execute! (CollectionOf meet-sfoon))
		(Set (Predicate "foo"))))

(test-end tname)

(opencog-test-end)
