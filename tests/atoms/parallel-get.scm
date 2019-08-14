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

(define expected-members
 (Get
  (Member
   (Variable "$BALL")
   (Variable "$SET"))))

