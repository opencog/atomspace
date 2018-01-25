(define vardecl
(VariableList
  (TypedVariableLink
    (VariableNode "$x-0-47189056")
    (TypeNode "VariableNode")
  )
  (TypedVariableLink
    (VariableNode "$x-1-518dbc2b")
    (TypeNode "VariableNode")
  )
  (VariableNode "$g-body-2653368")
)
)

(define ill-quoted
  (QuoteLink
    (LambdaLink
      (UnquoteLink
        (VariableList
          (VariableNode "$x-0-47189056")
          (VariableNode "$x-1-518dbc2b")
        )
      )
      (UnquoteLink
        (VariableNode "$g-body-2653368")
      )
    )
  )
)

(define well-quoted
  (QuoteLink
    (LambdaLink
      (UnquoteLink
        (VariableList
          (VariableNode "$x-0-47189056")
          (VariableNode "$x-1-518dbc2b")
        )
      )
      (UnquoteLink
        (VariableNode "$g-body-2653368")
      )
    )
  )
)
