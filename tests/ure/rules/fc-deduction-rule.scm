;; =============================================================================
;; Crisp logic entailment (Deduction) Rule.
;;
;;   A->B
;;   B->C
;;   |-
;;   A->C
;;
;; See examples/ure/README.md for more details.
;; -----------------------------------------------------------------------------

(define fc-deduction-rule
    (BindLink
        (VariableList
            (TypedVariableLink
                (VariableNode "$A")
                (TypeNode "ConceptNode"))
            (TypedVariableLink
                (VariableNode "$B")
                (TypeNode "ConceptNode"))
            (TypedVariableLink
                (VariableNode "$C")
                (TypeNode "ConceptNode")))
        (AndLink
            (PresentLink
                (InheritanceLink
                    (VariableNode "$A")
                    (VariableNode "$B")
                )
                (InheritanceLink
                    (VariableNode "$B")
                    (VariableNode "$C")
                )
            )
            ;; To avoid matching (Inheritance A B) and (Inheritance B A)
            (NotLink
                (IdenticalLink
                    (VariableNode "$A")
                    (VariableNode "$C")
                )
            )
        )
        (ExecutionOutputLink
            (GroundedSchemaNode "scm: fc-deduction-formula")
            (ListLink
                (InheritanceLink
                    (VariableNode "$A")
                    (VariableNode "$C"))
                (InheritanceLink
                    (VariableNode "$A")
                    (VariableNode "$B"))
                (InheritanceLink
                    (VariableNode "$B")
                    (VariableNode "$C"))
            )
        )
    )
)


;; -----------------------------------------------------------------------------
;; Deduction Formula
;;
;; If both confidence and strength of A->B and B->C are above 0.5 then
;; set the TV of A->C to (stv 1 1)
;; -----------------------------------------------------------------------------

(define (fc-deduction-formula AC AB BC)
    (let (  (sAB (cog-mean AB))
            (cAB (cog-confidence AB))
            (sBC (cog-mean BC))
            (cBC (cog-confidence BC)))
        (if (and (>= sAB 0.5) (>= cAB 0.5) (>= sBC 0.5) (>= cBC 0.5))
            (cog-set-tv! AC (stv 1 1)))))

;; Associate a name to the rule
(define fc-deduction-rule-name
    (DefinedSchemaNode "fc-deduction-rule"))
(DefineLink
    fc-deduction-rule-name
    fc-deduction-rule)
