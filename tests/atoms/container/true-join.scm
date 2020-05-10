;
; true-join.scm
; JoinLink unit test. Performs an actual join, for real,
; i.e. an actual set intersection of the filters.

; Data
(Evaluation (Predicate "Pa") (List (Concept "A")))
(Evaluation (Predicate "Pab") (List (Concept "A") (Concept "B")))
(Evaluation (Predicate "Pabc")
	(List (Concept "A") (Concept "B") (Concept "C")))

(define filter-ab
	(MaximalJoin
		(VariableList
			(TypedVariable (Variable "X") (Signature (Concept "A")))
			(TypedVariable (Variable "Y") (Signature (Concept "B"))))))
