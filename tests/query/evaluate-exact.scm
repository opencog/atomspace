(define answer1 (ListLink (EqualLink (ConceptNode "dog") (ConceptNode "cat"))))
(define answer2 (ListLink (EqualLink (ConceptNode "dog") (ConceptNode "dog"))))

(define MatchAll (BindLink
    (VariableList (stv 1.000000 0.000000)
        (VariableNode "$A")
        (VariableNode "$B")
    )
    (ListLink (stv 1.000000 1.000000)
        (EqualLink
            (VariableNode "$A")
            (VariableNode "$B")
        )
    )
    (ListLink
        (VariableNode "$A")
        (VariableNode "$B")
    )
))

(define MatchOne (BindLink
    (VariableList (stv 1.000000 0.000000)
        (VariableNode "$A")
        (VariableNode "$B")
    )
    (AndLink
        (NotLink
            (EqualLink
                (VariableNode "$A")
                (VariableNode "$B")
            )
        )
        (ListLink (stv 1.000000 1.000000)
            (EqualLink
                (VariableNode "$A")
                (VariableNode "$B")
            )
        )
    )
    (ListLink
        (VariableNode "$A")
        (VariableNode "$B")
    )
))

