(define quoted-exec
   (LambdaLink
      (VariableList
         (VariableNode "$vardecl")
         (VariableNode "$body")
      )
      (QuoteLink
         (ExecutionOutputLink
            (UnquoteLink
               (VariableNode "$vardecl")
            )
            (UnquoteLink
               (VariableNode "$body")
            )
         )
      )
   )
)

(define put-quoted-exec
(PutLink
   (LambdaLink
      (VariableNode "$X")
   )
   quoted-exec)
)
