(use-modules (opencog))
(use-modules (opencog exec))

;; Facts
(Evaluation (stv 1 1)
 (Predicate "are-friends")
 (List
  (Concept "John")
  (Concept "Mary")))

(Evaluation (stv 1 1)
 (Predicate "are-friends")
 (List
  (Concept "Mary")
  (Concept "Edward")))

(Evaluation (stv 1 1)
 (Predicate "are-friends")
 (List
  (Concept "Michel")
  (Concept "Edward")))

(Evaluation (stv 1 1)
 (Predicate "are-friends")
 (List
  (Concept "Cyril")
  (Concept "John")))

(Evaluation (stv 1 1)
 (Predicate "is-musician")
 (Concept "John"))

(Evaluation (stv 1 1)
 (Predicate "is-musician")
 (Concept "Mary"))

(Evaluation (stv 1 1)
 (Predicate "is-musician")
 (Concept "Edward"))

;; Rule
(define mixed-clauses
 (BindLink
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

   (EvaluationLink (stv 1.000000 1.000000)
    (PredicateNode "is-musician")
    (ConceptNode "John"))
  )

  (VariableNode "$who")
 ))
