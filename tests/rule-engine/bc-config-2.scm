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

(EquivalenceLink
   (Node "pln-rule-modus-ponens")
   pln-rule-modus-ponens
)

(EquivalenceLink
   (Node "pln-rule-deduction")
   pln-rule-deduction
)

; Termination criteria parameters
(ExecutionLink
   (SchemaNode "URE:maximum-iterations")
   (ConceptNode "URE")
   (NumberNode "100")
)

