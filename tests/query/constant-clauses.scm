(use-modules (opencog))
(use-modules (opencog exec))

;; Facts
(Evaluation
 (Predicate "are-friends")
 (List
  (Concept "John")
  (Concept "Mary")))

(Evaluation
 (Predicate "are-friends")
 (List
  (Concept "Mary")
  (Concept "Edward")))

(Evaluation
 (Predicate "are-friends")
 (List
  (Concept "Michel")
  (Concept "Edward")))

(Evaluation
 (Predicate "are-friends")
 (List
  (Concept "Cyril")
  (Concept "John")))

(Evaluation
 (Predicate "is-musician")
 (Concept "John"))

(Evaluation
 (Predicate "is-musician")
 (Concept "Mary"))

(Evaluation
 (Predicate "is-musician")
 (Concept "Edward"))

;; Rule
(define mixed-clauses
 (CollectionOf
 (QueryLink
  (VariableList
   (TypedVariableLink
    (VariableNode "$who")
    (TypeNode "ConceptNode"))
   (TypedVariableLink
    (VariableNode "$Y")
    (TypeNode "ConceptNode")))

  (AndLink
   (EvaluationLink
    (PredicateNode "is-musician")
    (VariableNode "$Y"))

   (EvaluationLink
    (PredicateNode "are-friends")
    (ListLink
     (VariableNode "$Y")
     (VariableNode "$who")))

   (EvaluationLink
    (PredicateNode "is-musician")
    (ConceptNode "John"))
  )

  (VariableNode "$who")
 )
 ))
