;
; Most users assume basic classical logic. Lets make sure they get that.
;
(use-modules (opencog) (opencog exec))

(State (Concept "me") (Predicate "hungry"))
(State (Concept "you") (Predicate "bored"))
(State (Concept "her") (Predicate "tired"))

(define double-a
(CollectionOf (Meet
	(TypedVariable (Variable "s") (Type 'Concept))
	(And
		(Present (Variable "s"))
		(Absent (State (Variable "s") (Predicate "hungry")))))))

(define double-b
(CollectionOf (Meet
	(TypedVariable (Variable "s") (Type 'Concept))
	(And
		(Present (Variable "s"))
		(Not (Present (State (Variable "s") (Predicate "hungry"))))))))

(define double-c
(CollectionOf (Meet
	(TypedVariable (Variable "s") (Type 'Concept))
	(And
		(Present (Variable "s"))
		(Not (And (Present (State (Variable "s") (Predicate "hungry")))))))))

(define double-d
(CollectionOf (Meet
	(TypedVariable (Variable "s") (Type 'Concept))
	(And
		(Present (Variable "s"))
		(Not (Or (Present (State (Variable "s") (Predicate "hungry")))))))))

(define double-e
(CollectionOf (Meet
	(TypedVariable (Variable "s") (Type 'Concept))
	(And
		(Present (Variable "s"))
		(And (Not (Present (State (Variable "s") (Predicate "hungry")))))))))

(define double-f
(CollectionOf (Meet
	(TypedVariable (Variable "s") (Type 'Concept))
	(And
		(Present (Variable "s"))
		(Or (Not (Present (State (Variable "s") (Predicate "hungry")))))))))

(define single-a
(CollectionOf (Meet
	(TypedVariable (Variable "s") (Type 'Concept))
	(Absent (State (Variable "s") (Predicate "hungry"))))))

(define single-b
(CollectionOf (Meet
	(TypedVariable (Variable "s") (Type 'Concept))
	(Not (Present (State (Variable "s") (Predicate "hungry")))))))

(define single-c
(CollectionOf (Meet
	(TypedVariable (Variable "s") (Type 'Concept))
	(Not (And (Present (State (Variable "s") (Predicate "hungry"))))))))

(define single-d
(CollectionOf (Meet
	(TypedVariable (Variable "s") (Type 'Concept))
	(Not (Or (Present (State (Variable "s") (Predicate "hungry"))))))))

(define single-e
(CollectionOf (Meet
	(TypedVariable (Variable "s") (Type 'Concept))
	(And (Not (Present (State (Variable "s") (Predicate "hungry"))))))))

(define single-f
(CollectionOf (Meet
	(TypedVariable (Variable "s") (Type 'Concept))
	(Or (Not (Present (State (Variable "s") (Predicate "hungry"))))))))
