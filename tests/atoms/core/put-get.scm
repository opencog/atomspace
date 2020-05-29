(Inheritance (Concept "B") (Concept "Parent"))
(Inheritance (Concept "C") (Concept "Parent"))

(define put-get
 (PutLink
   (EvaluationLink
    (PredicateNode "relation")
    (ListLink
     (VariableNode "x")
     (ConceptNode "A")))
  (GetLink
   (Inheritance (Variable "$X") (Concept "Parent")))))

(define expected-put-get
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
