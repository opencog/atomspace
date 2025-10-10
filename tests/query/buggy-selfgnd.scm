;
; In this case, the instantiator tried to instantiate and add a new
; QueryLink atom to Atomspace, throwing exception. More details here:
; https://github.com/opencog/atomspace/issues/210

(EvaluationLink
    (ConceptNode "arkle")
    (ConceptNode "barkle")
    (ConceptNode "curry"))

(EvaluationLink
    (ConceptNode "glib")
    (ConceptNode "blab"))

; This is a kind-of-ish poorly-formed expression.
; When the EqualLink is evaluated, its arguments are executed first.
; If $lnk was bound to the QueryLink (which it could be, because there
; is no type restriction on $lnk), this causes the QueryLink to be run
; again .. and again .. infinite regress.
;
; To avoid infinite regress, use IdenticalLink instead of EqualLink.
(define bnd
    (CollectionOf
    (QueryLink
        (AndLink
            (VariableNode "$lnk")
            (EvaluationLink
                (VariableNode "$a")
                (VariableNode "$b"))
            ; (EqualLink
            (IdenticalLink
                (VariableNode "$lnk")
                (EvaluationLink
                    (VariableNode "$a")
                    (VariableNode "$b")
                )
            ))
        (VariableNode "$lnk")
    )
    )
)
