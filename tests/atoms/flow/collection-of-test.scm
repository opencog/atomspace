;
; collection-of-test.scm -- Verify that CollectionOf works.
;

(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))

(opencog-test-runner)
(define tname "collection-of-test")
(test-begin tname)

(Evaluation (Predicate "foo") (Concept "bar"))

(define get-qry
	(Get
		(TypedVariable (Variable "X") (Type 'Concept))
		(Evaluation (Predicate "foo") (Variable "X"))))

(define set-qry
	(CollectionOf
		(Meet
			(TypedVariable (Variable "X") (Type 'Concept))
			(Evaluation (Predicate "foo") (Variable "X")))))

(test-assert "get set"
	(equal? (cog-execute! get-qry) (Set (Concept "bar"))))

(test-assert "collection set"
	(equal? (cog-execute! set-qry) (Set (Concept "bar"))))

(test-end tname)

(opencog-test-end)
