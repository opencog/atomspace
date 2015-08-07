; =============================================================================
; Deduction Rule.
;
; A->B
; B->C
; |-
; A->C
;
; See examples/rule-engine/README.md for more details.
; -----------------------------------------------------------------------------

(define crisp-deduction-rule
    (BindLink
        (VariableList
            (VariableNode "$A")
            (VariableNode "$B")
            (VariableNode "$C")
            )
        (AndLink
            (ImplicationLink
                (VariableNode "$A")
                (VariableNode "$B")
            )
            (ImplicationLink
                (VariableNode "$B")
                (VariableNode "$C")
            )
            ; To avoid matching (Implication A B) and (Implication B A)
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
                    (VariableNode "$B"))
                (ImplicationLink
                    (VariableNode "$B")
                    (VariableNode "$C")
                )
                (ImplicationLink
                    (VariableNode "$A")
                    (VariableNode "$C")
                )
            )
        )
    )
)


; -----------------------------------------------------------------------------
; Deduction Formula
;
; If both confidence and strength of A->B and B->C are above 0.5 then
; set the TV of A->C to (stv 1 1)
; -----------------------------------------------------------------------------

(define (crisp-deduction-formula AB BC AC)
    (let
        ((sAB (cog-stv-strength AB))
         (cAB (cog-stv-confidence AB))
         (sBC (cog-stv-strength BC))
         (cBC (cog-stv-confidence BC)))
      (if (and (>= sAB 0.5) (>= cAB 0.5) (>= sBC 0.5) (>= cBC 0.5))
          (cog-set-tv! AC (stv 1 1)))))

; Associate a name to the rule
(define crisp-deduction-rule-name (Node "crisp-deduction-rule"))
(DefineLink
  crisp-deduction-rule-name
  crisp-deduction-rule)
