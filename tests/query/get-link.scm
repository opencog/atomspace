;
; Data and tests for GetLink
;

(use-modules ((opencog query)))

(InheritanceLink (ConceptNode "Ben") (ConceptNode "human"))
(InheritanceLink (ConceptNode "Linas") (ConceptNode "human"))
(InheritanceLink (ConceptNode "Sparky") (ConceptNode "dog"))

(define is-human
	(GetLink (InheritanceLink (VariableNode "$H") (ConceptNode "human"))))

;; two variables, including type restrictions
(define is-something
	(GetLink
		(VariableList
			(TypedVariableLink (VariableNode "$A") (TypeNode "ConceptNode"))
			(VariableNode "$B"))
		(InheritanceLink (VariableNode "$A") (VariableNode "$B"))))

;; looks for queries.
;; This binds only the second variable, thus, the first variable
;; remains free. Running this should results in a grounding to is-human,
;; above. That is, it should find
;; (SetLink (ConceptNode "human"))
(define is-query
	(GetLink
		(VariableNode "$B") ;; bind only the second variable.
		(InheritanceLink (VariableNode "$H") (VariableNode "$B"))))

(define g-take-contain
   (GetLink
      (VariableList
         (TypedVariableLink
            (VariableNode "$X")
            (TypeNode "ConceptNode")
         )
         (TypedVariableLink
            (VariableNode "$Z")
            (TypeNode "ConceptNode")
         )
      )
      (AndLink
         (EvaluationLink
            (PredicateNode "take")
            (ListLink
               (VariableNode "$X")
               (ConceptNode "treatment-1")
            )
         )
         (EvaluationLink
            (PredicateNode "contain")
            (ListLink
               (ConceptNode "treatment-1")
               (VariableNode "$Z")
            )
         )
      )
   )
)

(EvaluationLink (stv 1 1)
   (PredicateNode "take")
   (ListLink
      (ConceptNode "John")
      (ConceptNode "treatment-1")))

(EvaluationLink (stv 1 1)
   (PredicateNode "contain")
   (ListLink
      (ConceptNode "treatment-1")
      (ConceptNode "compound-A")))
