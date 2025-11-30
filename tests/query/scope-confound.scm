;
; Data from bug opencog/atomspace #933
;

(use-modules (opencog))
(use-modules (opencog exec))

; -------------------------------------------------------------
; From "opencog/pln/rules/evaluation-to-member-rule.scm"
(define (evaluation-to-member-2-rule-loose)
    (CollectionOf
    (QueryLink
        ; var decls
        (VariableList
            (VariableNode "$A")
            (VariableNode "$B")
            (TypedVariableLink
                (VariableNode "$D")
                (TypeNode "PredicateNode")))

        ; pattern
        (EdgeLink
            (VariableNode "$D")
            (ListLink
                (VariableNode "$A")
                (VariableNode "$B")))

        ; result ...
        (ExecutionOutputLink
            (GroundedSchemaNode "scm: evaluation-to-member-2-formula")
                (ListLink
                    (TagLink
                        (VariableNode "$A")
                        (ScopeLink
                            (VariableNode "$X")
                            (EdgeLink
                                (VariableNode "$D")
                                (ListLink
                                    (VariableNode "$X")
                                    (VariableNode "$B")))))

                    (TagLink
                        (VariableNode "$B")
                        (ScopeLink
                            (VariableNode "$Y")
                            (EdgeLink
                                (VariableNode "$D")
                                (ListLink
                                    (VariableNode "$A")
                                    (VariableNode "$Y")))))
                    (EdgeLink
                        (VariableNode "$D")
                        (ListLink
                            (VariableNode "$A")
                            (VariableNode "$B")))))
    )
    ))


(define (evaluation-to-member-2-rule)
    (CollectionOf
    (QueryLink
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
        (EdgeLink
            (VariableNode "$D")
            (ListLink
                (VariableNode "$A")
                (VariableNode "$B")))

        ; result ...
        (ExecutionOutputLink
            (GroundedSchemaNode "scm: evaluation-to-member-2-formula")
                (ListLink
                    (TagLink
                        (VariableNode "$A")
                        (ScopeLink
                            (VariableNode "$X")
                            (EdgeLink
                                (VariableNode "$D")
                                (ListLink
                                    (VariableNode "$X")
                                    (VariableNode "$B")))))

                    (TagLink
                        (VariableNode "$B")
                        (ScopeLink
                            (VariableNode "$Y")
                            (EdgeLink
                                (VariableNode "$D")
                                (ListLink
                                    (VariableNode "$A")
                                    (VariableNode "$Y")))))
                    (EdgeLink
                        (VariableNode "$D")
                        (ListLink
                            (VariableNode "$A")
                            (VariableNode "$B")))))
    )
    ))


(define (evaluation-to-member-2-formula MAXDXB MBXDAX DAB) MAXDXB)

;; -----------------------------------------------------------
