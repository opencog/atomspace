;
; Most users assume basic classical logic. Lets make sure they get that.
;
(use-modules (opencog) (opencog exec))

(State (Concept "me") (Predicate "hungry"))
(State (Concept "you") (Predicate "bored"))
(State (Concept "her") (Predicate "tired"))

(define double-a
(Get
	(TypedVariable (Variable "s") (Type 'Concept))
	(And
		(Present (Variable "s"))
		(Absent (State (Variable "s") (Predicate "hungry"))))))

(define double-b
(Get
	(TypedVariable (Variable "s") (Type 'Concept))
	(And
		(Present (Variable "s"))
		(Not (Present (State (Variable "s") (Predicate "hungry")))))))

(define double-c
(Get
	(TypedVariable (Variable "s") (Type 'Concept))
	(And
		(Present (Variable "s"))
		(Not (And (Present (State (Variable "s") (Predicate "hungry"))))))))

(define double-d
(Get
	(TypedVariable (Variable "s") (Type 'Concept))
	(And
		(Present (Variable "s"))
		(Not (Or (Present (State (Variable "s") (Predicate "hungry"))))))))

(define double-e
(Get
	(TypedVariable (Variable "s") (Type 'Concept))
	(And
		(Present (Variable "s"))
		(And (Not (Present (State (Variable "s") (Predicate "hungry"))))))))

(define double-f
(Get
	(TypedVariable (Variable "s") (Type 'Concept))
	(And
		(Present (Variable "s"))
		(Or (Not (Present (State (Variable "s") (Predicate "hungry"))))))))

(define single-a
(Get
	(TypedVariable (Variable "s") (Type 'Concept))
	(Absent (State (Variable "s") (Predicate "hungry")))))

(define single-b
(Get
	(TypedVariable (Variable "s") (Type 'Concept))
	(Not (Present (State (Variable "s") (Predicate "hungry"))))))

(define single-c
(Get
	(TypedVariable (Variable "s") (Type 'Concept))
	(Not (And (Present (State (Variable "s") (Predicate "hungry")))))))

(define single-d
(Get
	(TypedVariable (Variable "s") (Type 'Concept))
	(Not (Or (Present (State (Variable "s") (Predicate "hungry")))))))

(define single-e
(Get
	(TypedVariable (Variable "s") (Type 'Concept))
	(And (Not (Present (State (Variable "s") (Predicate "hungry")))))))

(define single-f
(Get
	(TypedVariable (Variable "s") (Type 'Concept))
	(Or (Not (Present (State (Variable "s") (Predicate "hungry")))))))
