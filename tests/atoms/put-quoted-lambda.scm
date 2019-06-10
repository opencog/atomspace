(define put-quoted-conjuction-lambda
(Put
  (VariableList
    (Variable "$vardecl")
    (Variable "$clause-1")
    (Variable "$clause-2"))
  (Quote
    (Lambda
      (Unquote (Variable "$vardecl"))
      (And
        (Unquote (Variable "$clause-1"))
        (Unquote (Variable "$clause-2")))))
  (List
    (VariableList (Variable "$X") (Variable "$Y"))
    (Inheritance (Concept "A") (Variable "$X"))
    (Inheritance (Concept "$X") (Variable "$Y"))))
)

(define put-quoted-conjuction-lambda-result
(LambdaLink
   (VariableList
      (VariableNode "$X")
      (VariableNode "$Y")
   )
   (AndLink
      (InheritanceLink
         (ConceptNode "$X")
         (VariableNode "$Y")
      )
      (InheritanceLink
         (ConceptNode "A")
         (VariableNode "$X")
      )
   )
)
)
