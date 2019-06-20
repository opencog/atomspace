;; =============================================================================
;; Deduction Rule.
;;
;; A->B
;; B->C
;; |-
;; A->C
;;
;; See examples/ure/README.md for more details.
;; -----------------------------------------------------------------------------

(define crisp-deduction-rule
    (BindLink
        (VariableList
            (VariableNode "$A")
            (VariableNode "$B")
            (VariableNode "$C")
        )
        (AndLink
            (PresentLink
                (ImplicationLink
                    (VariableNode "$A")
                    (VariableNode "$B")
                )
                (ImplicationLink
                    (VariableNode "$B")
                    (VariableNode "$C")
                )
            )
            ;; To avoid matching (Implication A B) and (Implication B A)
            (NotLink
                (EqualLink
                    (VariableNode "$A")
                    (VariableNode "$C")
                )
            )
        )
        (ExecutionOutputLink
            (GroundedSchemaNode "scm: crisp-deduction-formula")
            (ListLink
                (ImplicationLink
                    (VariableNode "$A")
                    (VariableNode "$C"))
                (ImplicationLink
                    (VariableNode "$A")
                    (VariableNode "$B"))
                (ImplicationLink
                    (VariableNode "$B")
                    (VariableNode "$C"))))))

;; -----------------------------------------------------------------------------
;; Deduction Formula
;;
;; If both confidence and strength of A->B and B->C are above 0.5 then
;; set the TV of A->C to (stv 1 1)
;; -----------------------------------------------------------------------------

(define (crisp-deduction-formula AC AB BC)
    (let
        ((sAB (cog-mean AB))
         (cAB (cog-confidence AB))
         (sBC (cog-mean BC))
         (cBC (cog-confidence BC)))
      (if (and (>= sAB 0.5) (>= cAB 0.5) (>= sBC 0.5) (>= cBC 0.5))
          (cog-set-tv! AC (stv 1 1)))))

;; Associate a name to the rule
(define crisp-deduction-rule-name
  (DefinedSchemaNode "crisp-deduction-rule"))
(DefineLink
  crisp-deduction-rule-name
  crisp-deduction-rule)
