;
; disco-dancers.scm
;
; Test the addition of implicit clauses for variables appearing in
; virtual clauses but not otherwise groundable.  Per bug #2516
;

(use-modules (opencog))
(use-modules (opencog exec))

(Concept "alice")
(Concept "bob")

(define variables (VariableList
    (TypedVariable
        (Variable "person1")
        (Type "ConceptNode"))
    (TypedVariable
        (Variable "person2")
        (Type "ConceptNode"))))

(define target (Not (Identical
                        (Variable "person1")
                        (Variable "person2") )))

; This should not throw...
(define (get-dancers) (Get variables target))

; This should work...
; (cog-execute! (get-dancers))
