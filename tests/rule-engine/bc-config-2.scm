(load "bc-modus-ponens.scm")
(load "bc-deduction.scm")

(MemberLink (stv 1 1)
    (Node "pln-rule-modus-ponens")
    (ConceptNode "URE")
)

(MemberLink (stv 1 1)
    (Node "pln-rule-deduction")
    (ConceptNode "URE")
)

; Termination criteria parameters
(ExecutionLink
   (SchemaNode "URE:maximum-iterations")
   (ConceptNode "URE")
   (NumberNode "100")
)

