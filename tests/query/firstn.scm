; Socrates, Einstein, and Peirce are all men.
(InheritanceLink
    (ConceptNode "Socrates")
    (ConceptNode "man"))

(InheritanceLink
    (ConceptNode "Einstein")
    (ConceptNode "man"))

(InheritanceLink
    (ConceptNode "Peirce")
    (ConceptNode "man"))

; Cat, dog and mouse are all animal.
(InheritanceLink
    (ConceptNode "cat")
    (ConceptNode "animal"))

(InheritanceLink
    (ConceptNode "dog")
    (ConceptNode "animal"))

(InheritanceLink
    (ConceptNode "mouse")
    (ConceptNode "animal"))

; Simple GetLink
(define get-men
    (GetLink
        (InheritanceLink
            (VariableNode "$X")
            (ConceptNode "man"))
    ))

; GetLink with disconnected clauses
(define get-disconnected
    (GetLink
        (AndLink
            (InheritanceLink
                (VariableNode "$X")
                (ConceptNode "man"))
            (InheritanceLink
                (VariableNode "$Y")
                (ConceptNode "animal")))
    ))

; Simple BindLink
(define bind-men
    (BindLink
        (VariableList (VariableNode "$X"))
        (InheritanceLink
            (VariableNode "$X")
            (ConceptNode "man"))
        (VariableNode "$X")
    ))

; BindLink with disconnected clauses
(define bind-disconnected
    (BindLink
        (VariableList
            (VariableNode "$X")
            (VariableNode "$Y"))
        (AndLink
            (InheritanceLink
                (VariableNode "$X")
                (ConceptNode "man"))
            (InheritanceLink
                (VariableNode "$Y")
                (ConceptNode "animal")))
        (ListLink
            (VariableNode "$X")
            (VariableNode "$Y"))
    ))
