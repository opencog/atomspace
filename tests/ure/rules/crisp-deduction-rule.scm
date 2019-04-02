;; =============================================================================
;; Simple Crisp Deduction Rule
;;
;; <LinkType>
;;   A
;;   B
;; <LinkType>
;;   B
;;   C
;; |-
;; <LinkType>
;;   A
;;   C
;; -----------------------------------------------------------------------------

(define (gen-crisp-deduction-rule link-type)
  (let* ((A (Variable "$A"))
         (B (Variable "$B"))
         (C (Variable "$C"))
         (AB (link-type A B))
         (BC (link-type B C))
         (AC (link-type A C))
         (vardecl (VariableList A B C))
         (precon1 (Evaluation (GroundedPredicate "scm: true-enough") AB))
         (precon2 (Evaluation (GroundedPredicate "scm: true-enough") BC))
         (precon3 (Not (Identical A C)))
         (pattern (And AB BC precon1 precon2 precon3))
         (rewrite (ExecutionOutput
                     (GroundedSchema "scm: crisp-deduction-formula")
                     (List AC AB BC))))
    (Bind
       vardecl
       pattern
       rewrite)))

(define crisp-deduction-inheritance-rule
  (gen-crisp-deduction-rule InheritanceLink))

(define crisp-deduction-implication-rule
  (gen-crisp-deduction-rule ImplicationLink))

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
(define crisp-deduction-inheritance-rule-name
  (DefinedSchema "crisp-deduction-inheritance-rule"))
(DefineLink
  crisp-deduction-inheritance-rule-name
  crisp-deduction-inheritance-rule)

(define crisp-deduction-implication-rule-name
  (DefinedSchema "crisp-deduction-implication-rule"))
(DefineLink
  crisp-deduction-implication-rule-name
  crisp-deduction-implication-rule)
