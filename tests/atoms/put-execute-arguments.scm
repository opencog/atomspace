(Inheritance (Concept "B") (Concept "Parent"))
(Inheritance (Concept "C") (Concept "Parent"))

(define put-execute-arguments
 (PutLink
   (EvaluationLink
    (PredicateNode "relation")
    (ListLink
     (VariableNode "x")
     (ConceptNode "A")))
  (GetLink
   (Inheritance (Variable "$X") (Concept "Parent")))))

(define expected-put-execute-arguments
 (SetLink
  (EvaluationLink
   (PredicateNode "relation")
   (ListLink
    (ConceptNode "B")
    (ConceptNode "A")))
  (EvaluationLink
   (PredicateNode "relation")
   (ListLink
    (ConceptNode "C")
    (ConceptNode "A")))))
