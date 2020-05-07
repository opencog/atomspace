;
; join.scm
; JoinLink unit test.

(Member (Concept "A") (Concept "S"))
(Evaluation (Predicate "P") (List (Concept "A")))

(define join
	(Join
		(TypedVariable (Variable "X") (Concept "A"))
		(PresentLink (Variable "X"))))
