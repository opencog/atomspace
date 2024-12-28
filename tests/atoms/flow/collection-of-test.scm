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
; Base function: static non-executable stuff.

(define set-abc
	(CollectionOf
		(List (Concept "a") (Concept "b") (Concept "c"))))

(test-assert "set-abc"
	(equal? (cog-execute! set-abc)
		(Set (Concept "a") (Concept "b") (Concept "c"))))

(define unordered-abc
	(CollectionOf
		(Type 'UnorderedLink)
		(List (Concept "a") (Concept "b") (Concept "c"))))

(test-assert "unordered-abc"
	(equal? (cog-execute! unordered-abc)
		(Unordered (Concept "a") (Concept "b") (Concept "c"))))

(define link-value-abc
	(CollectionOf
		(Type 'LinkValue)
		(List (Concept "a") (Concept "b") (Concept "c"))))

(test-assert "link-value-abc"
	(equal? (cog-execute! link-value-abc)
		(LinkValue (Concept "a") (Concept "b") (Concept "c"))))

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
	(CollectionOf
		(Type 'EdgeLink)
		(Get
			(TypedVariable (Variable "X") (Type 'Concept))
			(Evaluation (Predicate "foo") (Variable "X")))))

(define meet-qry-edge
	(CollectionOf
		(Type 'EdgeLink)
		(Meet
			(TypedVariable (Variable "X") (Type 'Concept))
			(Evaluation (Predicate "foo") (Variable "X")))))

(define qry-edge (cog-execute! get-qry-edge))
(format #t "Get edge is ~A" qry-edge)

(define meet-edge (cog-execute! meet-qry-edge))
(format #t "Meet edge is ~A" meet-edge)

(test-assert "get edge"
	(equal? qry-edge (Edge (Concept "bar"))))

(test-assert "meet edge"
	(equal? meet-edge (Edge (Concept "bar"))))

; -------------------------------------------------------------
(test-end tname)

(opencog-test-end)
