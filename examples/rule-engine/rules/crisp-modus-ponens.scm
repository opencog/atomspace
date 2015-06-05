; =============================================================================
; Crisp Modus Ponens Rule.
;
; A->B
; A
; |-
; B
;
; See examples/rule-engine/README.md for more details.
; -----------------------------------------------------------------------------

(define crisp-modus-ponens
    (BindLink
        (VariableList
            (VariableNode "$A")
            (VariableNode "$B"))
        (AndLink
            (ImplicationLink
                (VariableNode "$A")
                (VariableNode "$B"))
            (VariableNode "$A"))
        (ExecutionOutputLink
            (GroundedSchemaNode "scm: crisp-modus-ponens-formula")
            (ListLink
                (ImplicationLink
                    (VariableNode "$A")
                    (VariableNode "$B"))
                (VariableNode "$A")
                (VariableNode "$B")))))

; -----------------------------------------------------------------------------
; Crisp Modus Ponens Formula
;
; If both confidence and strength of A->B and A are above 0.5 then set
; the TV of B to (stv 1 1)
; -----------------------------------------------------------------------------

(define (crisp-modus-ponens-formula AB A B)
    (let
        ((sA (cog-stv-strength A))
         (cA (cog-stv-confidence A))
         (sAB (cog-stv-strength AB))
         (cAB (cog-stv-confidence AB)))
      (if (and (>= sA 0.5) (>= cA 0.5) (>= sAB 0.5) (>= cAB 0.5))
          (cog-set-tv! B (stv 1 1)))))

; Useful to set this rule as a member of a rule base
(EquivalenceLink
   (Node "crisp-rule-base-modus-ponens-rule")
   crisp-modus-ponens
)
