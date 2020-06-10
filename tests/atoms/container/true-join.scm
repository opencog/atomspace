;
; true-join.scm
; JoinLink unit test. Performs an actual join, for real,
; i.e. an actual set intersection of the filters.

(use-modules (opencog) (opencog exec))

; Data
(Evaluation (Predicate "Pa") (List (Concept "A")))
(Evaluation (Predicate "Pab") (List (Concept "A") (Concept "B")))
(Evaluation (Predicate "Pabc")
	(List (Concept "A") (Concept "B") (Concept "C")))

(define min-filter-ab
	(MinimalJoin
		(VariableList
			(TypedVariable (Variable "X") (Signature (Concept "A")))
			(TypedVariable (Variable "Y") (Signature (Concept "B"))))))

; (cog-execute! min-filter-ab)

(define max-filter-ab
	(MaximalJoin
		(VariableList
			(TypedVariable (Variable "X") (Signature (Concept "A")))
			(TypedVariable (Variable "Y") (Signature (Concept "B"))))))

(define min-filter-ap
	(MinimalJoin
		(VariableList
			(TypedVariable (Variable "Y") (Signature (Concept "B")))
			(TypedVariable (Variable "P") (Type 'PredicateNode)))))

(define max-filter-ap
	(MaximalJoin
		(VariableList
			(TypedVariable (Variable "Y") (Signature (Concept "B")))
			(TypedVariable (Variable "P") (Type 'PredicateNode)))))

; ---------------------------------
; Same as above, but with constants

(define min-const-ab
	(MinimalJoin
		(Present (Concept "A"))
		(Present (Concept "B"))))

(define max-const-ab
	(MaximalJoin
		(Present (Concept "A"))
		(Present (Concept "B"))))

(define min-const-ap
	(MinimalJoin
		(TypedVariable (Variable "P") (Type 'PredicateNode))
		(Present (Concept "B"))))

; (cog-execute! min-const-ap)

(define max-const-ap
	(MaximalJoin
		(TypedVariable (Variable "P") (Type 'PredicateNode))
		(Present (Concept "B"))))

; (cog-execute! max-const-ap)
