(use-modules (opencog query))
(use-modules (opencog rule-engine))


;;;;;;;;;;;;;;;;;;;;
;; Knowledge base ;;
;;;;;;;;;;;;;;;;;;;;

(ImplicationScope (stv 1.0 1.0)
   (TypedVariable
      (Variable "$X")
      (Type "ConceptNode"))
   (And
      (Evaluation
         (Predicate "croaks")
         (Variable "$X"))
      (Evaluation
         (Predicate "eats_flies")
         (Variable "$X")))
   (Inheritance
      (Variable "$X")
      (Concept "Frog")))

(ImplicationScope (stv 1.0 1.0)
   (TypedVariable
      (Variable "$X")
      (Type "ConceptNode"))
   (And
      (Evaluation
         (Predicate "chirps")
         (Variable "$X"))
      (Evaluation
         (Predicate "sings")
         (Variable "$X")))
   (Inheritance
      (Variable "$X")
      (Concept "Canary")))

(ImplicationScope (stv 1.0 1.0)
   (TypedVariable
      (Variable "$X")
      (Type "ConceptNode"))
   (Inheritance
      (Variable "$X")
      (Concept "Frog"))
   (Inheritance
      (Variable "$X")
      (Concept "green")))

(ImplicationScope (stv 1.0 1.0)
   (TypedVariable
      (Variable "$X")
      (Type "ConceptNode"))
   (Inheritance
      (Variable "$X")
      (Concept "Canary"))
   (Inheritance
      (Variable "$X")
      (Concept "yellow")))

(Evaluation (stv 1.0 1.0)
   (Predicate "croaks")
   (Concept "Fritz"))

(Evaluation (stv 1.0 1.0)
   (Predicate "chirps")
   (Concept "Tweety"))

(Inheritance (stv 1.0 1.0)
   (Concept "Tweety")
   (Concept "yellow"))

(Evaluation (stv 1.0 1.0)
   (Predicate "eats_flies")
   (Concept "Tweety"))

(Evaluation (stv 1.0 1.0)
   (Predicate "eats_flies")
   (Concept "Fritz"))

;;;;;;;;;;;;;;;
;; Rule base ;;
;;;;;;;;;;;;;;;

(define rule1
	(BindLink
		(VariableList
			(VariableNode "$x")
		)
		(AndLink
			(InheritanceLink
				(VariableNode "$x")
				(ConceptNode "croaks")
			)
			(EvaluationLink
				(PredicateNode "eats")
				(ListLink
					(VariableNode "$x")
					(ConceptNode "flies")
				)
			)
		)
		(InheritanceLink
			(VariableNode "$x")
			(ConceptNode "frog")
		)
	)
)

(define rule1-name (DefinedSchemaNode "rule1"))
(DefineLink rule1-name rule1)

(define rule2
	(BindLink
		(VariableList
			(VariableNode "$x")
		)
		(InheritanceLink
			(VariableNode "$x")
			(ConceptNode "frog")
		)
		(InheritanceLink
			(VariableNode "$x")
			(ConceptNode "green")
		)
	)
)

(define rule2-name (DefinedSchemaNode "rule2"))
(DefineLink rule2-name rule2)

(define source
	(InheritanceLink
		(ConceptNode "fritz")
		(ConceptNode "croaks")
	)
)

(EvaluationLink
	(PredicateNode "eats")
	(ListLink
		(ConceptNode "fritz")
		(ConceptNode "flies")
	)
)

;-------------------------------------------
(define wiki (ConceptNode "wikipedia-fc"))

(InheritanceLink  ; Defining a rule base
	(ConceptNode "wikipedia-fc")
	(ConceptNode "URE")
)

(ExecutionLink
   (SchemaNode "URE:maximum-iterations")
   (ConceptNode "wikipedia-fc")
   (NumberNode 20)
)

(MemberLink (stv 0.9 1)
	rule1-name
	(ConceptNode "wikipedia-fc")
)

(MemberLink (stv 0.5 1)
	rule2-name
	(ConceptNode "wikipedia-fc")
)
