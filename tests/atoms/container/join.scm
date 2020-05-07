;
; join.scm
; JoinLink unit test.

(use-modules (opencog) (opencog exec))

(Member (Concept "A") (Concept "S"))
(Evaluation (Predicate "P") (List (Concept "A")))

(define max-join
	(MaximalJoin
		(TypedVariable (Variable "X") (Signature (Concept "A")))
		(Present (Variable "X"))))

(define min-join
	(MinimalJoin
		(TypedVariable (Variable "X") (Signature (Concept "A")))
		(Present (Variable "X"))))

(define max-replace
	(MaximalJoin
		(TypedVariable (Variable "X") (Signature (Concept "A")))
		(Present (Variable "X"))
		(Replacement (Variable "X") (Concept "B"))))

(define min-replace
	(MinimalJoin
		(TypedVariable (Variable "X") (Signature (Concept "A")))
		(Present (Variable "X"))
		(Replacement (Variable "X") (Concept "B"))))
