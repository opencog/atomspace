;
; unify.scm -- Term unification demo
;
; The query engine is able to perform conventional, classical
; term unification. The below is a very short, simple demo of
; this ability.
;
(use-modules (opencog) (opencog exec))

; Populate the AtomSpace with some initial data. This includes
; the data we want to match, and also some confounding data,
; that should not be found.
(Inheritance (Concept "A") (Concept "B"))
(Inheritance (Concept "A") (Concept "C"))
(Inheritance (Concept "B") (Concept "C"))

; Define a basic unifier. It uses the conventional GetLink to
; do the work.
(define unifier
	(Get
		(VariableList (Variable "$X") (Variable "$Y"))
		(Identical
			(Inheritance (Concept "A") (Variable "$Y"))
			(Inheritance (Variable "$X") (Concept "B")))))

; Run it.
(cog-execute! unifier)

; The variable declaration is not explicitly required; the variables
; will be automatically extracted in the order that they are found.
; Caution: this reverses the variable order from the above! So $Y
; comes first, so the results will be reversed.
(define implicit-vars
	(Get
		(Identical
			(Inheritance (Concept "A") (Variable "$Y"))
			(Inheritance (Variable "$X") (Concept "B")))))

; Run it.
(cog-execute! implicit-vars)

; The End. That's all, folks!
