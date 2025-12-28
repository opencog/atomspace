(define quoted-exec
   (LambdaLink
      (VariableList
         (VariableNode "$gsn")
         (VariableNode "$arg")
      )
      (QuoteLink
         (ExecutionOutputLink
            (UnquoteLink
               (VariableNode "$gsn")
            )
            (UnquoteLink
               (VariableNode "$arg")
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

(define quoted-eval
   (LambdaLink
      (VariableList
         (VariableNode "$gpn")
         (VariableNode "$arg")
      )
      (QuoteLink
         (EvaluationLink
            (UnquoteLink
               (VariableNode "$gpn")
            )
            (UnquoteLink
               (VariableNode "$arg")
            )
         )
      )
   )
)

(define put-quoted-eval
(PutLink
   (LambdaLink
      (VariableNode "$X")
   )
   quoted-eval)
)
