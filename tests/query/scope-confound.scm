;
; Data from bug opencog/atomspace #933
;

(use-modules (opencog))
(use-modules (opencog query))

; -------------------------------------------------------------
; From "opencog/pln/rules/evaluation-to-member-rule.scm"
(define (evaluation-to-member-2-rule)
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
                        (SatisfyingSetLink
                            (VariableNode "$X")
                            (EvaluationLink
                                (VariableNode "$D")
                                (ListLink
                                    (VariableNode "$X")
                                    (VariableNode "$B")))))

                    (MemberLink
                        (VariableNode "$B")
                        (SatisfyingSetLink
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


(define (evaluation-to-member-2-formula MAXDXB MBXDAX DAB)
    (List (Node "A place holder")))

;; -----------------------------------------------------------
