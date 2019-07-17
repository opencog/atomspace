(Inheritance (Concept "B") (Concept "Parent"))
(Inheritance (Concept "C") (Concept "Parent"))

(define put-execute-arguments
 (PutLink
  (LambdaLink
   (VariableNode "x")
   (EvaluationLink
    (PredicateNode "relation")
    (ListLink
     (VariableNode "x")
     (VariableNode "A"))))
  (GetLink
   (Inheritance (Variable "$X") (Concept "Parent")))))

(define expected-put-execute-arguments
 (SetLink
  (EvaluationLink
   (PredicateNode "relation")
   (ListLink
    (ConceptNode "B")
    (VariableNode "A")))
  (EvaluationLink
   (PredicateNode "relation")
   (ListLink
    (ConceptNode "C")
    (VariableNode "A")))))