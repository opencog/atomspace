;
; Test Data from bug opencog/atomspace #910
;

(use-modules (opencog))
(use-modules (opencog exec))

; (use-modules (opencog logger))
; (cog-logger-set-level! "fine")
; (cog-logger-set-stdout! #t)
; (cog-logger-set-timestamp! #f)

;; ----------------------------------------------------------------
;; Helper functions

(define (content-1)
   (MemberLink
      (ConceptNode "Socrates@81011e61-27a7-4001-a63b-3b569478bced")
      (ScopeLink
         (VariableNode "$X")
         (EvaluationLink
            (PredicateNode "breathe@ea723bda-70bb-47c0-8930-b344fb47a4d1")
            (ListLink
               (VariableNode "$X")
               (ConceptNode "air@76357ee3-d334-4b5f-b49e-323b294310b6")
            )
         )
      )
   )
)


(define (content-2)
; MemberLink having the same Variable name as rule
   (MemberLink
      (ConceptNode "Socrates@81011e61-27a7-4001-a63b-3b569478bced")
      (ScopeLink
         (VariableNode "$X-M2E")
         (EvaluationLink
            (PredicateNode "breathe@ea723bda-70bb-47c0-8930-b344fb47a4d1")
            (ListLink
               (VariableNode "$X-M2E")
               (ConceptNode "air@76357ee3-d334-4b5f-b49e-323b294310b6")
            )
         )
      )
   )
)


(define (member-to-evaluation-2-1-rule)
   (BindLink
      (VariableList
         (VariableNode "$B")
         (VariableNode "$C")
         (TypedVariableLink
            (VariableNode "$D")
            (TypeNode "PredicateNode")
         )
      )
      (MemberLink
         (VariableNode "$B")
         (ScopeLink
            (VariableNode "$X-M2E")
            (EvaluationLink
               (VariableNode "$D")
               (ListLink
                  (VariableNode "$X-M2E")
                  (VariableNode "$C")
               )
            )
         )
      )
      (ExecutionOutputLink
         (GroundedSchemaNode "scm: member-to-evaluation-formula")
         (ListLink
            (EvaluationLink
               (VariableNode "$D")
               (ListLink
                  (VariableNode "$B")
                  (VariableNode "$C")
               )
            )
            (MemberLink
               (VariableNode "$B")
               (ScopeLink
                  (VariableNode "$X-M2E")
                  (EvaluationLink
                     (VariableNode "$D")
                     (ListLink
                        (VariableNode "$X-M2E")
                        (VariableNode "$C")
                     )
                  )
               )
            )
         )
      )
   )
)

; Same as above, but apha-renamed deduction. Should get the same
; results.
(define (member-to-evaluation-2-1-alt)
   (BindLink
      (VariableList
         (VariableNode "$B")
         (VariableNode "$C")
         (TypedVariableLink
            (VariableNode "$D")
            (TypeNode "PredicateNode")
         )
      )
      (MemberLink
         (VariableNode "$B")
         (ScopeLink
            (VariableNode "$X-M2E")
            (EvaluationLink
               (VariableNode "$D")
               (ListLink
                  (VariableNode "$X-M2E")
                  (VariableNode "$C")
               )
            )
         )
      )
      (ExecutionOutputLink
         (GroundedSchemaNode "scm: member-to-evaluation-formula")
         (ListLink
            (EvaluationLink
               (VariableNode "$D")
               (ListLink
                  (VariableNode "$B")
                  (VariableNode "$C")
               )
            )
            (MemberLink
               (VariableNode "$B")
               (ScopeLink
                  (VariableNode "$some-bound-var")
                  (EvaluationLink
                     (VariableNode "$D")
                     (ListLink
                        (VariableNode "$some-bound-var")
                        (VariableNode "$C")
                     )
                  )
               )
            )
         )
      )
   )
)

(define (member-to-evaluation-formula EVAL MEM)
   (cog-set-tv! EVAL (cog-tv MEM)))
