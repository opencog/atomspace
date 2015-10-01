; In this case instatiator tried to instantiate and add new BindLink
; atom to Atomspace throwing exception. More details here:
; https://github.com/opencog/atomspace/issues/210

(EvaluationLink
    (ConceptNode "arkle")
    (ConceptNode "barkle")
    (ConceptNode "curry"))

(EvaluationLink
    (ConceptNode "glib")
    (ConceptNode "blab"))

(define bnd
    (BindLink
        (AndLink
            (VariableNode "$lnk")
            (EvaluationLink
                (VariableNode "$a")
                (VariableNode "$b"))
            (EqualLink
                (VariableNode "$lnk")
                (EvaluationLink
                    (VariableNode "$a")
                    (VariableNode "$b")
                )
            ))
        (VariableNode "$lnk")
    )
)
