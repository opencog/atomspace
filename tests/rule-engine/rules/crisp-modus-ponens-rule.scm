; =============================================================================
; Crisp Modus Ponens entailment rule.
;
; A
; A->B
; |-
; B
;
; See examples/rule-engine/README.md for more details.
; -----------------------------------------------------------------------------

(define crisp-modus-ponens-rule
  (let* ((A (Variable "$A"))
         (B (Variable "$B"))
         (AB (Implication A B))
         (LambdaT (Type "LambdaLink"))
         (PredicateT (Type "PredicateNode"))
         (vardecl (VariableList
                     (TypedVariable A (TypeChoice LambdaT PredicateT))
                     (TypedVariable B (TypeChoice LambdaT PredicateT))))
         (precon1 (Evaluation (GroundedPredicate "scm: true-enough") A))
         (precon2 (Evaluation (GroundedPredicate "scm: true-enough") AB))
         (pattern (And AB precon1 precon2))
         (rewrite (ExecutionOutput
                     (GroundedSchema "scm: crisp-modus-ponens-formula")
                     (List B A AB))))
    (BindLink
        vardecl
        pattern
        rewrite)))

; -----------------------------------------------------------------------------
; Crisp Modus Ponens truth-value formula
;
; If both confidence and strength of A->B and A are above 0.5 then set
; the TV of B to (stv 1 1)
; -----------------------------------------------------------------------------

(define (true-enough-bool a)
  (let ((s (cog-stv-strength a)) (c (cog-stv-confidence a)))
    (and (> s 0.5) (> c 0.5))))

(define (true-enough a)
  (bool->tv (true-enough-bool a)))

(define (crisp-modus-ponens-formula B A AB)
  (if (and (true-enough-bool A) (true-enough-bool AB))
      (cog-set-tv! B (stv 1 1))))

; Associate a name to the rule
(define crisp-modus-ponens-rule-name
    (DefinedSchemaNode "crisp-modus-ponens-rule"))

(DefineLink
    crisp-modus-ponens-rule-name
    crisp-modus-ponens-rule)
