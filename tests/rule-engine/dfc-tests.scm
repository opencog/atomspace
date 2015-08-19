; Substitution case where $A = Cat and $B = Animal
(define deduction-ab-substitute-1
    (BindLink
        (VariableList                
                (TypedVariableLink
                   (VariableNode "$C")
                   (TypeNode "ConceptNode")))
        (AndLink
            (InheritanceLink
                (ConceptNode "Cat")
                (ConceptNode "Animal")
            )
            (InheritanceLink
                (ConceptNode "Animal")
                (VariableNode "$C")
            )
            ; To avoid matching (Inheritance A B) and (Inheritance B A)
            (NotLink
                (EqualLink
                    (ConceptNode "Cat")
                    (VariableNode "$C")
                )
            )
        )
        (ExecutionOutputLink
            (GroundedSchemaNode "scm: pln-formula-simple-deduction")
            (ListLink
                (InheritanceLink
                    (ConceptNode "Cat")
                    (ConceptNode "Animal"))
                (InheritanceLink
                    (ConceptNode "Animal")
                    (VariableNode "$C")
                )
                (InheritanceLink
                    (ConceptNode "Cat")
                    (VariableNode "$C")
                )
            )
        )
    )
  )

; Substitution case where $B = Cat and $C = Animal
(define deduction-ab-substitute-2
    (BindLink
        (VariableList
                (TypedVariableLink
                   (VariableNode "$A")
                   (TypeNode "ConceptNode")))
        (AndLink
            (InheritanceLink
                (VariableNode "$A")
                (ConceptNode  "Cat")
            )
            (InheritanceLink
                (ConceptNode  "Cat")
                (ConceptNode  "Animal")
            )
            ; To avoid matching (Inheritance A B) and (Inheritance B A)
            (NotLink
                (EqualLink
                    (VariableNode "$A")
                    (ConceptNode  "Animal")
                )
            )
        )
        (ExecutionOutputLink
            (GroundedSchemaNode "scm: pln-formula-simple-deduction")
            (ListLink
                (InheritanceLink
                    (VariableNode "$A")
                    (ConceptNode  "Cat"))
                (InheritanceLink
                    (ConceptNode  "Cat")
                    (ConceptNode  "Animal")
                )
                (InheritanceLink
                    (VariableNode "$A")
                    (ConceptNode  "Animal")
                )
            )
        )
    )
  )