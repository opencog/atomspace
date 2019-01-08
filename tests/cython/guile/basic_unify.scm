;
; Basic test of unification
;
; Create some misc atoms.

(use-modules (opencog))
(use-modules (opencog exec))

(define (stv mean conf) (cog-new-stv mean conf))

(ListLink
	(ConceptNode "hello" (stv 0.5 0.5))
	(ConceptNode "world")
)

(define wobbly
	(ConceptNode "wobbly" (stv 0.5 0.5))
)

(InheritanceLink (ConceptNode "Frog") (ConceptNode "animal"))
(InheritanceLink (ConceptNode "Zebra") (ConceptNode "animal"))
(InheritanceLink (ConceptNode "Deer") (ConceptNode "animal"))
(InheritanceLink (ConceptNode "Spaceship") (ConceptNode "machine"))

(define find-animals
	(BindLink
		(VariableNode "$var")
		(InheritanceLink
			(VariableNode "$var")
			(ConceptNode "animal")
		)
		(VariableNode "$var")
	)
)
