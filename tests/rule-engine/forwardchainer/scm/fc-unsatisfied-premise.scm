(define (return-value x) x)

(define rule
 (BindLink

  (VariableList
   (TypedVariable (Variable "$a") (Type "ConceptNode"))
   (TypedVariable (Variable "$b") (Type "ConceptNode")))

  (And
   (Inheritance
    (Variable "$a")
    (Variable "$b"))
   (Member
    (Variable "$a")
    (Variable "$b")))

  (ExecutionOutput
   (GroundedSchemaNode "scm: return-value")
   (Set (Variable "$a") (Variable "$b")))))

(define rule-name (DefinedSchema "rule"))

(Define rule-name rule)

(define rule-base (Concept "rule-base"))

(Inheritance rule-base (Concept "URE"))

(ure-add-rule rule-base rule-name)
