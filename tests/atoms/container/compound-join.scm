;
; compound-join.scm
; CompoundJoinUTest
;

(use-modules (opencog) (opencog exec))

; Data
(Evaluation (Predicate "above") (List (Concept "sky")))
(Evaluation (Predicate "part of") (List (Concept "beach") (Concept "sand")))
(Evaluation (Predicate "part of") (List (Concept "beach") (Concept "sea")))
(Evaluation (Predicate "planetary")
    (List (Concept "sky") (Concept "ground") (Concept "ocean")))

(define min-compound
	(MinimalJoin
		(VariableList
			(TypedVariable (Variable "X") (Type 'ConceptNode))
			(TypedVariable (Variable "Y") (Type 'ConceptNode)))
		(Present (List (Variable "X") (Variable "Y")))))

(define max-compound
	(MaximalJoin
		(VariableList
			(TypedVariable (Variable "X") (Type 'ConceptNode))
			(TypedVariable (Variable "Y") (Type 'ConceptNode)))
		(Present (List (Variable "X") (Variable "Y")))))
