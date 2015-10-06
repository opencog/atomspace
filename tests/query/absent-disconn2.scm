(define soln (SetLink (ListLink (ConceptNode "A") (ConceptNode "B"))))

(define test
    (BindLink
        (VariableList
            (TypedVariableLink (VariableNode "$var1") (TypeNode "ConceptNode"))
            (TypedVariableLink (VariableNode "$var2") (TypeNode "ConceptNode"))
            (TypedVariableLink (VariableNode "$var3") (TypeNode "ConceptNode"))
        )
        (AndLink
            (ListLink
                (VariableNode "$var1")
                (VariableNode "$var2")
            )
            (AbsentLink
                (ListLink
                    (VariableNode "$var3")
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
