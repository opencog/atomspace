(define put-1
(Put
  (Lambda (Inheritance (Variable "$X") (Variable "$Y")))
  (List (Variable "$Z") (Variable "$Z"))))

(define expected-1
(LambdaLink
  (VariableNode "$Z")
  (InheritanceLink
    (VariableNode "$Z")
    (VariableNode "$Z"))))

(define put-2
(Put
  (Inheritance (Variable "$X") (Variable "$Y"))
  (List (Variable "$Z") (Variable "$Z"))))

(define expected-2
(LambdaLink
  (VariableNode "$Z")
  (InheritanceLink
    (VariableNode "$Z")
    (VariableNode "$Z"))))

(define put-3
(Put
  (Lambda
    (Inheritance (Variable "$X") (Variable "$Y")))
    (List (Variable "$Z") (Variable "$W"))))

(define expected-3
(LambdaLink
  (InheritanceLink
    (VariableNode "$X")
    (VariableNode "$Y"))))

(define put-4
(Put
  (Inheritance (Variable "$X") (Variable "$Y"))
  (List (Variable "$Z") (Variable "$W"))))

(define expected-4
(LambdaLink
  (InheritanceLink
    (VariableNode "$X")
    (VariableNode "$Y"))))

(define put-5
(Put
  (List
    (Variable "$spe-arg-0")
    (Concept "D"))
  (Lambda
    (VariableList
      (Variable "$sha-arg-0")
      (Variable "$sha-arg-1"))
    (Inheritance
      (Variable "$sha-arg-0")
      (Variable "$sha-arg-1")))))

(define expected-5
(Lambda
  (VariableList
    (Variable "$sha-arg-0")
    (Variable "$sha-arg-1"))
  (List
    (Inheritance
      (Variable "$sha-arg-0")
      (Variable "$sha-arg-1"))
    (Concept "D"))))
