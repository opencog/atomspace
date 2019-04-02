;; =============================================================================
;; Simple Crisp Deduction Rule
;;
;; Inheritance A B
;; Inheritance B C
;; |-
;; Inheritance A C
;; -----------------------------------------------------------------------------

(define bc-deduction-rule
  (let* ((A (Variable "$A"))
         (B (Variable "$B"))
         (C (Variable "$C"))
         (AB (Inheritance A B))
         (BC (Inheritance B C))
         (AC (Inheritance A C))
         (Concept (Type "ConceptNode"))
         (vardecl (VariableList
                     (TypedVariable A Concept)
                     (TypedVariable B Concept)
                     (TypedVariable C Concept)))
         (precon1 (Evaluation (GroundedPredicate "scm: true-enough") AB))
         (precon2 (Evaluation (GroundedPredicate "scm: true-enough") BC))
         (precon3 (Not (Identical A C)))
         (pattern (And AB BC precon1 precon2 precon3))
         (rewrite (ExecutionOutput
                     (GroundedSchema "scm: bc-deduction-formula")
                     (List AC AB BC))))
    (Bind
       vardecl
       pattern
       rewrite)))

; -----------------------------------------------------------------------------
; Deduction Formula
; -----------------------------------------------------------------------------

(define (true-enough-bool a)
  (let ((s (cog-stv-strength a)) (c (cog-stv-confidence a)))
    (and (> s 0.5) (> c 0.5))))

(define (true-enough a)
  (bool->tv (true-enough-bool a)))

(define (bc-deduction-formula AC AB BC)
  ;; We keep this precondition here again just in case
  (if (and (true-enough-bool AB) (true-enough-bool BC))
      (cog-set-tv! AC (stv 1 1))))

; Associate a name to the rule
(define bc-deduction-rule-name (DefinedSchema "bc-deduction-rule"))
(DefineLink
  bc-deduction-rule-name
  bc-deduction-rule)
