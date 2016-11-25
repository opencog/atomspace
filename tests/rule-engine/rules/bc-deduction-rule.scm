;; =============================================================================
;; Simple Crisp Deduction Rule
;;
;; Inheritance A B
;; Inheritance B C
;; |-
;; Inheritance A C
;; -----------------------------------------------------------------------------

(define bc-deduction-rule
  (let* ((A (VariableNode "$A"))
         (B (VariableNode "$B"))
         (C (VariableNode "$C"))
         (AB (Inheritance A B))
         (BC (Inheritance B C))
         (AC (Inheritance A C))
         (Concept (TypeNode "ConceptNode"))
         (vardecl (VariableList
                     (TypedVariableLink A Concept)
                     (TypedVariableLink B Concept)
                     (TypedVariableLink C Concept)))
         (precon1 (Evaluation (GroundedPredicate "scm: true-enough") AB))
         (precon2 (Evaluation (GroundedPredicate "scm: true-enough") BC))
         (precon3 (Not (Identical A C)))
         (pattern (And AB BC precon1 precon2 precon3))
         (rewrite (ExecutionOutputLink
                     (GroundedSchemaNode "scm: bc-deduction-formula")
                     (ListLink AB BC AC))))
    (Bind
       vardecl
       pattern
       rewrite)))

; -----------------------------------------------------------------------------
; Deduction Formula
; -----------------------------------------------------------------------------

(define (true-enough a)
  (let ((s (cog-stv-strength a)) (c (cog-stv-confidence a)))
    (bool->tv (and (>= s 0.5) (> c 0)))))

(define (bc-deduction-formula AB BC AC)
  ;; We keep this precondition here again just in case
  (if (and (tv->bool (true-enough AB)) (tv->bool (true-enough BC)))
      (cog-set-tv! AC (stv 1 1))))

; Associate a name to the rule
(define bc-deduction-rule-name (DefinedSchema "bc-deduction-rule"))
(DefineLink
  bc-deduction-rule-name
  bc-deduction-rule)
