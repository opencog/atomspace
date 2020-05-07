;
; join.scm
; JoinLink unit test.

(use-modules (opencog) (opencog exec))

(Member (Concept "A") (Concept "S"))
(Evaluation (Predicate "P") (List (Concept "A")))

(define join
	(Join
		(TypedVariable (Variable "X") (Signature (Concept "A")))
		(PresentLink (Variable "X"))))
