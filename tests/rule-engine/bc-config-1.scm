(load "bc-modus-ponens.scm")

(MemberLink (stv 1 1)
    pln-rule-modus-ponens
    (ConceptNode "URE")
)

; Termination criteria parameters
(ExecutionLink
   (SchemaNode "URE:maximum-iterations")
   (ConceptNode "URE")
   (NumberNode "10")
)
