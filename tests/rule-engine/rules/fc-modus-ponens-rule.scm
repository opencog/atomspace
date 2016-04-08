; =============================================================================
; Crisp Modus Ponens entailment rule.
;
;     A->B and A |- B
;
; See examples/rule-engine/README.md for more details.
; -----------------------------------------------------------------------------

(define fc-modus-ponens-rule
    (BindLink
        (VariableList
            (TypedVariable
                (VariableNode "$A")
                (TypeChoice
                    (Type "LambdaLink")
                    (Type "PredicateNode")))
            (TypedVariable
                (VariableNode "$B")
                (TypeChoice
                    (Type "LambdaLink")
                    (Type "PredicateNode"))))
        (AndLink
            (ImplicationLink
                (VariableNode "$A")
                (VariableNode "$B"))
            (VariableNode "$A"))
        (ExecutionOutputLink
            (GroundedSchemaNode "scm: fc-modus-ponens-formula")
            (ListLink
                (VariableNode "$A")
                (ImplicationLink
                    (VariableNode "$A")
                    (VariableNode "$B"))
                (VariableNode "$B")))))

; -----------------------------------------------------------------------------
; Crisp Modus Ponens truth-value formula
;
; If both confidence and strength of A->B and A are above 0.5 then set
; the TV of B to (stv 1 1)
; -----------------------------------------------------------------------------

(define (fc-modus-ponens-formula A AB B)
    (let (  (sA (cog-stv-strength A))
            (cA (cog-stv-confidence A))
            (sAB (cog-stv-strength AB))
            (cAB (cog-stv-confidence AB)))
        (if (and (>= sA 0.5) (>= cA 0.5) (>= sAB 0.5) (>= cAB 0.5))
            (cog-set-tv! B (stv 1 1)))))

; Associate a name to the rule
(define fc-modus-ponens-rule-name
    (DefinedSchemaNode "fc-modus-ponens-rule"))

(DefineLink
    fc-modus-ponens-rule-name
    fc-modus-ponens-rule)
