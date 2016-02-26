; =============================================================================
; Modus Ponens Rule
;
; Given P(A implies B) and sA, calculate sB
; -----------------------------------------------------------------------------

(define bc-modus-ponens-rule
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
            (GroundedSchemaNode "scm: bc-modus-ponens-formula")
            (ListLink
                (VariableNode "$B")
                (ImplicationLink
                    (VariableNode "$A")
                    (VariableNode "$B"))))))

; -----------------------------------------------------------------------------
; Modus Ponens Formula
; -----------------------------------------------------------------------------

; -----------------------------------------------------------------------------
; Side-effect: TruthValue of AC may be updated
; -----------------------------------------------------------------------------

(define (bc-modus-ponens-formula B AB)
    (cog-set-tv!
        B
        (bc-modus-ponens-side-effect-free-formula
            AB)))

; -----------------------------------------------------------------------------
; This version has no side effects and simply returns a TruthValue
; -----------------------------------------------------------------------------

(define (bc-modus-ponens-side-effect-free-formula AB)
    (let
        ((sA (cog-stv-strength (gar AB)))
         (cA (cog-stv-confidence (gar AB))))
            (stv                          ; Strength
                (*
                    (cog-stv-strength AB)
                    sA)
                (+                        ; Confidence
                    (cog-stv-confidence AB)
                    cA))))

; Associate a name to the rule
(define bc-modus-ponens-rule-name (DefinedSchemaNode "bc-modus-ponens-rule"))
(DefineLink
  bc-modus-ponens-rule-name
  bc-modus-ponens-rule)

; =============================================================================

