;
; Data and tests for GetLink with embedded variable decls.
;

(use-modules (opencog) (opencog exec))

(Inheritance (Concept "Ben") (Concept "human"))
(Inheritance (Concept "Linas") (Concept "human"))
(Inheritance (Concept "Sparky") (Concept "dog"))

(define is-human
	(GetLink
		(Present (Inheritance (Variable "$H") (Concept "human")))))

; (cog-execute! is-human)

;; Two variables, including type restrictions
(define is-something
	(GetLink
		(And
			(Present (Inheritance (Variable "$A") (Variable "$B")))
			(TypedVariable (Variable "$A") (Type 'Concept))
		)))

; (cog-execute! is-something)

;; An empty TypeChoice means the variable can have no type at all.
;; This is the same as the bottom type. It's also the same as
;;    (TypeChoice (Type 'Notype))
;; See https://github.com/opencog/atomspace/issues/2490
(define is-nothing
	(GetLink
		(And
			(Present (Inheritance (Variable "$H") (Concept "human")))
			(TypedVariable (Variable "$H") (TypeChoice))
		)))

; (cog-execute! is-nothing)

;; --------------------------------------------------------------

(define g-take-contain
   (Get
      (And
			(Present
         	(Evaluation
            	(Predicate "take")
            	(List (Variable "$X") (Concept "treatment-1"))))
         (TypedVariable (Variable "$X") (Type "Concept"))
			(Present
         	(Evaluation
            	(Predicate "contain")
            	(List (Concept "treatment-1") (Variable "$Z"))))
         (TypedVariable (Variable "$Z") (Type "Concept"))
      )))

; (cog-execute! g-take-contain)

(Evaluation
   (Predicate "take")
   (List (Concept "John") (Concept "treatment-1")))

(Evaluation
   (Predicate "contain")
   (List (Concept "treatment-1") (Concept "compound-A")))
