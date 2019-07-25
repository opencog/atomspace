;
; always.scm
;
; Basic unit test for AlwaysLink
; This is nearly identical to a demo file in the examples directory.
; Invented after discussions at opencog/atomspace#2203
;

(use-modules (opencog) (opencog exec))

; Three baskets holding balls
(Inheritance (Concept "reds basket") (Concept "basket"))
(Inheritance (Concept "reds&greens basket") (Concept "basket"))
(Inheritance (Concept "yellows basket") (Concept "basket"))

; Balls placed into baskets
(Member (Concept "red ball")      (Concept "reds basket"))
(Member (Concept "red ball too")  (Concept "reds basket"))
(Member (Concept "red ball also") (Concept "reds basket"))

(Member (Concept "red ball")     (Concept "reds&greens basket"))
(Member (Concept "red ball too") (Concept "reds&greens basket"))
(Member (Concept "green ball")   (Concept "reds&greens basket"))

(Member (Concept "yellow ball") (Concept "yellows basket"))
(Member (Concept "ochre ball")  (Concept "yellows basket"))

; Colors of the balls
(Evaluation (Predicate "is red") (Concept "red ball"))
(Evaluation (Predicate "is red") (Concept "red ball too"))
(Evaluation (Predicate "is red") (Concept "red ball also"))

(Evaluation (Predicate "is green")  (Concept "green ball"))

(Evaluation (Predicate "is yellow") (Concept "yellow ball"))
(Evaluation (Predicate "is yellow") (Concept "ochre ball"))

(define baskets-with-red-balls-only
	(Bind
		(VariableList
			(TypedVariable (Variable "basket") (Type 'ConceptNode))
			(TypedVariable (Variable "ball") (Type 'ConceptNode))
		)
		(And
			(Inheritance (Variable "basket") (Concept "basket"))
			(Member (Variable "ball") (Variable "basket"))

			; Always means that *every* ball in the basket MUST
			; be red! Failure to satisfy this invalidates the
			; search.
			(Always (Evaluation (Predicate "is red")  (Variable "ball")))
		)

		; Report the basket which has only red balls in it.
		(Variable "basket"))
)

;;; (cog-execute! baskets-with-red-balls-only)

(define baskets-with-same-color
(Bind
	(VariableList
		(TypedVariable (Variable "basket")      (Type 'ConceptNode))
		(TypedVariable (Variable "some ball")   (Type 'ConceptNode))
		(TypedVariable (Variable "other ball")  (Type 'ConceptNode))
		(TypedVariable (Variable "some color")  (Type 'PredicateNode))
		(TypedVariable (Variable "other color") (Type 'PredicateNode))
	)
	(And
		(Inheritance (Variable "basket")       (Concept "basket"))
		(Member (Variable "some ball")         (Variable "basket"))
		(Member (Variable "other ball")        (Variable "basket"))
		(Evaluation (Variable "some color")    (Variable "some ball"))
		(Evaluation (Variable "other color")   (Variable "other ball"))
		(Always (Equal (Variable "some color") (Variable "other color")))
	)
	(Variable "basket"))
)
