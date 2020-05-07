;
; join.scm
; JoinLink unit test.

(use-modules (opencog) (opencog exec))

(Member (Concept "A") (Concept "S"))
(Evaluation (Predicate "P") (List (Concept "A")))

(define max-join
	(MaximalJoin
		(TypedVariable (Variable "X") (Signature (Concept "A")))
		(PresentLink (Variable "X"))))

(define min-join
	(MinimalJoin
		(TypedVariable (Variable "X") (Signature (Concept "A")))
		(PresentLink (Variable "X"))))
