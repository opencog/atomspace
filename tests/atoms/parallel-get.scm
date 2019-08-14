(Inheritance (Concept "ball-1") (Concept "green"))
(Inheritance (Concept "ball-2") (Concept "red"))
(Inheritance (Concept "ball-3") (Concept "green"))
(Inheritance (Concept "ball-4") (Concept "red"))

(define parallel-get
 (ParallelGet
  (Variable "$BALL")
  (Inheritance
   (Variable "$BALL")
   (Concept "green"))
  (SetNode "S")))

(define expected-parallel-get
 (SetNode "S"))

(define parallel-put
 (Put
  (Variable "$BALL")
  (Inheritance
   (Variable "$BALL")
   (Concept "selected-balls"))
  (SetNode "S")))

(define expected-parallel-put
 (SetLink
  (InheritanceLink
   (ConceptNode "ball-1")
   (ConceptNode "selected-balls"))
  (InheritanceLink
   (ConceptNode "ball-3")
   (ConceptNode "selected-balls"))))
