;
; collection-of-test.scm -- Verify that CollectionOf works.
;

(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))

(opencog-test-runner)
(define tname "collection-of-test")
(test-begin tname)

(Evaluation (Predicate "foo") (Concept "bar"))

; -------------------------------------------------------------
; Base function: convert to set.

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

; -------------------------------------------------------------
; Same as above, but do type conversion.

(define get-qry-edge
	(Type 'EdgeLink)
	(Get
		(TypedVariable (Variable "X") (Type 'Concept))
		(Evaluation (Predicate "foo") (Variable "X"))))

(define set-qry-edge
	(Type 'EdgeLink)
	(CollectionOf
		(Meet
			(TypedVariable (Variable "X") (Type 'Concept))
			(Evaluation (Predicate "foo") (Variable "X")))))

(test-assert "get edge"
	(equal? (cog-execute! get-qry-edge) (Edge (Concept "bar"))))

(test-assert "collection edcge"
	(equal? (cog-execute! set-qry0edge) (Edge (Concept "bar"))))

(test-end tname)

(opencog-test-end)
