(use-modules (opencog) (opencog exec))

(define soln (SetLink))

(ListLink (ConceptNode "A") (ConceptNode "X"))

(define test-absent
    (BindLink
        (VariableList
            (TypedVariableLink (VariableNode "$var1") (TypeNode "ConceptNode"))
            (TypedVariableLink (VariableNode "$var2") (TypeNode "ConceptNode"))
        )
        (AndLink
            (ListLink
                (VariableNode "$var1")
                (VariableNode "$var2")
            )
            (AbsentLink
                (ListLink
                    (VariableNode "$var1")
                    (ConceptNode "X")
                )
            )
        )
        (ListLink
            (VariableNode "$var1")
            (VariableNode "$var2")
        )
    )
)

(define test-not-present
    (BindLink
        (VariableList
            (TypedVariableLink (VariableNode "$var1") (TypeNode "ConceptNode"))
            (TypedVariableLink (VariableNode "$var2") (TypeNode "ConceptNode"))
        )
        (AndLink
            (ListLink
                (VariableNode "$var1")
                (VariableNode "$var2")
            )
            (NotLink (PresentLink
                (ListLink
                    (VariableNode "$var1")
                    (ConceptNode "X")
                )
            ))
        )
        (ListLink
            (VariableNode "$var1")
            (VariableNode "$var2")
        )
    )
)
