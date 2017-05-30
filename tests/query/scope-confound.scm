;
; Data from bug opencog/atomspace #933
;

(use-modules (opencog))
(use-modules (opencog query))

; -------------------------------------------------------------
; From "opencog/pln/rules/evaluation-to-member-rule.scm"
(define (evaluation-to-member-2-rule-loose)
    (BindLink
        ; var decls
        (VariableList
            (VariableNode "$A")
            (VariableNode "$B")
            (TypedVariableLink
                (VariableNode "$D")
                (TypeNode "PredicateNode")))

        ; pattern
        (EvaluationLink
            (VariableNode "$D")
            (ListLink
                (VariableNode "$A")
                (VariableNode "$B")))

        ; result ...
        (ExecutionOutputLink
            (GroundedSchemaNode "scm: evaluation-to-member-2-formula")
                (ListLink
                    (MemberLink
                        (VariableNode "$A")
                        (SatisfyingSetScopeLink
                            (VariableNode "$X")
                            (EvaluationLink
                                (VariableNode "$D")
                                (ListLink
                                    (VariableNode "$X")
                                    (VariableNode "$B")))))

                    (MemberLink
                        (VariableNode "$B")
                        (SatisfyingSetScopeLink
                            (VariableNode "$Y")
                            (EvaluationLink
                                (VariableNode "$D")
                                (ListLink
                                    (VariableNode "$A")
                                    (VariableNode "$Y")))))
                    (EvaluationLink
                        (VariableNode "$D")
                        (ListLink
                            (VariableNode "$A")
                            (VariableNode "$B")))))))


(define (evaluation-to-member-2-rule)
    (BindLink
        ; var decls
        (VariableList
            (TypedVariableLink
                (VariableNode "$A")
                (TypeNode "ConceptNode"))
            (TypedVariableLink
                (VariableNode "$B")
                (TypeNode "ConceptNode"))
            (TypedVariableLink
                (VariableNode "$D")
                (TypeNode "PredicateNode")))

        ; pattern
        (EvaluationLink
            (VariableNode "$D")
            (ListLink
                (VariableNode "$A")
                (VariableNode "$B")))

        ; result ...
        (ExecutionOutputLink
            (GroundedSchemaNode "scm: evaluation-to-member-2-formula")
                (ListLink
                    (MemberLink
                        (VariableNode "$A")
                        (SatisfyingSetScopeLink
                            (VariableNode "$X")
                            (EvaluationLink
                                (VariableNode "$D")
                                (ListLink
                                    (VariableNode "$X")
                                    (VariableNode "$B")))))

                    (MemberLink
                        (VariableNode "$B")
                        (SatisfyingSetScopeLink
                            (VariableNode "$Y")
                            (EvaluationLink
                                (VariableNode "$D")
                                (ListLink
                                    (VariableNode "$A")
                                    (VariableNode "$Y")))))
                    (EvaluationLink
                        (VariableNode "$D")
                        (ListLink
                            (VariableNode "$A")
                            (VariableNode "$B")))))))


(define (evaluation-to-member-2-formula MAXDXB MBXDAX DAB) MAXDXB)

;; -----------------------------------------------------------
