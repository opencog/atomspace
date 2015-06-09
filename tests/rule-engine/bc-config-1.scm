(load "bc-modus-ponens.scm")

(MemberLink (stv 1 1)
    (Node "pln-rule-modus-ponens")
    (ConceptNode "URE")
)

(EquivalenceLink
   (Node "pln-rule-modus-ponens")
   pln-rule-modus-ponens
)

; Termination criteria parameters
(ExecutionLink
   (SchemaNode "URE:maximum-iterations")
   (ConceptNode "URE")
   (NumberNode "10")
)

