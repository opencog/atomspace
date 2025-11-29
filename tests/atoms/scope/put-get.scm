(Inheritance (Concept "B") (Concept "Parent"))
(Inheritance (Concept "C") (Concept "Parent"))

(define put-get
 (PutLink
   (EdgeLink
    (PredicateNode "relation")
    (ListLink
     (VariableNode "x")
     (ConceptNode "A")))
  (CollectionOf
   (MeetLink
    (Inheritance (Variable "$X") (Concept "Parent"))))))

(define expected-put-get
 (SetLink
  (EdgeLink
   (PredicateNode "relation")
   (ListLink
    (ConceptNode "B")
    (ConceptNode "A")))
  (EdgeLink
   (PredicateNode "relation")
   (ListLink
    (ConceptNode "C")
    (ConceptNode "A")))))
