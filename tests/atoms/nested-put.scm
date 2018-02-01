(define nested-put-1
(Put
  (Put
    (Lambda
      (Variable "$X")
      (Variable "$X"))
    (Lambda
      (VariableList
        (Variable "$Y")
        (Variable "$Z"))
      (Inheritance
        (Variable "$Y")
        (Variable "$Z"))))
  (List
    (Concept "A")
    (Variable "$X")))
)

(define expected-1
(Lambda
  (Variable "$X")
  (Inheritance
    (Concept "A")
    (Variable "$X")))
)

(define nested-put-2
(PutLink
  (PutLink
    (LambdaLink
      (VariableNode "$X")
      (VariableNode "$X")
    )
    (ListLink
      (ConceptNode "texts")
      (VariableNode "$W")
    )
  )
  (LambdaLink
    (VariableList
      (VariableNode "$Y")
      (VariableNode "$Z")
    )
    (InheritanceLink
      (VariableNode "$Y")
      (VariableNode "$Z")
    )
  )
)
)

(define expected-2
(Lambda
  (VariableList
    (VariableNode "$Y")
    (VariableNode "$Z")
  )
  (ListLink
    (ConceptNode "texts")
    (Inheritance
      (VariableNode "$Y")
      (VariableNode "$Z"))))
)

(define nested-put-3
(PutLink
  (PutLink
    (PutLink
      (LambdaLink
        (VariableNode "$X")
        (VariableNode "$X")
      )
      (ConceptNode "A")
    )
    (Concept "B")
  )
  (Concept "C")
)
)
