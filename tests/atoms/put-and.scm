(define top
  (LambdaLink
    (VariableNode "$top-arg")
    (VariableNode "$top-arg")))

(define inheritance-shallow-abstraction
  (LambdaLink
    (VariableList
      (VariableNode "$sha-arg-0")
      (VariableNode "$sha-arg-1"))
    (InheritanceLink
      (VariableNode "$sha-arg-0")
      (VariableNode "$sha-arg-1"))))

(define and-shallow-abstraction
  (LambdaLink
    (VariableList
      (VariableNode "$sha-arg-0")
      (VariableNode "$sha-arg-1"))
    (LocalQuoteLink
      (AndLink
        (VariableNode "$sha-arg-0")
        (VariableNode "$sha-arg-1")))))
