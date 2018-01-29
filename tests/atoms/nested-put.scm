(define nested-put
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

(define expected
(Lambda
  (Variable "$X")
  (Inheritance
    (Concept "A")
    (Variable "$X")))
)
