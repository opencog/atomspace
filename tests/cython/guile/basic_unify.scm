;
; Basic test of unification
;
; Create some misc atoms.

(use-modules (opencog))
(use-modules (opencog exec))

(ListLink (Concept "hello") (Concept "world"))
(cog-set-value! (Concept "hello") (Predicate "*-TruthValueKey-*") (FloatValue 0.5 0.5))

(define wobbly (Concept "wobbly"))
(cog-set-value! wobbly (Predicate "*-TruthValueKey-*") (FloatValue 0.5 0.5))

(InheritanceLink (ConceptNode "Frog") (ConceptNode "animal"))
(InheritanceLink (ConceptNode "Zebra") (ConceptNode "animal"))
(InheritanceLink (ConceptNode "Deer") (ConceptNode "animal"))
(InheritanceLink (ConceptNode "Spaceship") (ConceptNode "machine"))

(define find-animals
	(CollectionOf (Query
		(VariableNode "$var")
		(InheritanceLink
			(VariableNode "$var")
			(ConceptNode "animal"))
		(VariableNode "$var"))))
