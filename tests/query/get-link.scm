;
; Data and tests for GetLink
;

(use-modules ((opencog exec)))

(InheritanceLink (ConceptNode "Ben") (ConceptNode "human"))
(InheritanceLink (ConceptNode "Linas") (ConceptNode "human"))
(InheritanceLink (ConceptNode "Sparky") (ConceptNode "dog"))

(define is-human
	(GetLink (InheritanceLink (VariableNode "$H") (ConceptNode "human"))))

;; Two variables, including type restrictions
(define is-something
	(GetLink
		(VariableList
			(TypedVariableLink (VariableNode "$A") (TypeNode "ConceptNode"))
			(VariableNode "$B"))
		(InheritanceLink (VariableNode "$A") (VariableNode "$B"))))

;; An empty TypeChoice means the variable can have no type at all.
;; This is the same as the bottom type. It's also the same as
;;    (TypeChoice (Type 'Notype))
;; See https://github.com/opencog/atomspace/issues/2490
(define is-nothing
	(GetLink
		(TypedVariableLink (VariableNode "$H") (TypeChoice))
		(InheritanceLink (VariableNode "$H") (ConceptNode "human"))))

;; Look for queries.
;; This binds only the second variable, thus, the first variable
;; remains free. Running this should results in a grounding to is-human,
;; above. That is, it should find
;; (SetLink (ConceptNode "human"))
(define is-query
	(GetLink
		(VariableNode "$B") ;; bind only the second variable.
		(InheritanceLink (VariableNode "$H") (VariableNode "$B"))))

;; --------------------------------------------------------------

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
