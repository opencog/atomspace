
(use-modules (opencog) (opencog exec))

(define get5 (GetLink
  (VariableList
   (TypedVariable (Variable "$num1") (Type 'NumberNode))
   (TypedVariable (Variable "$num2") (Type 'NumberNode)))
   (And
      (Present (Variable "$num1"))
      (Present (Variable "$num2"))
      (Present (GreaterThan (Variable "$num1") (Variable "$num2"))))))

; Expected result from running the above
(define ans5 (SetLink
   (ListLink
      (NumberNode 3)
      (NumberNode 2))))

; Pattern to match
(GreaterThanLink (Number 3) (Number 2))

; Confounding AtomSpace content
(Number 0) (Number 1) (Number 2) (Number 3) (Number 4) (Number 5)

; (cog-execute! g5)

*unspecified*
