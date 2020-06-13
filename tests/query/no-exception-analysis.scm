;
; no-exception-analysis.scm
;
; Backward-chainer creates something like the following.
; It triggered an exception during pattern analysis.
; If the MeetLink can be creted without a throw, then test passes.
;
(use-modules (opencog) (opencog exec))

(define (meet)
(Meet
  (VariableList
    (TypedVariable (Variable "$who") (Type "ConceptNode"))
    (TypedVariable (Variable "$B-7e054a97") (Type "ConceptNode"))
    (TypedVariable (Variable "$y-281a7166") (Type "ConceptNode"))
    (TypedVariable (Variable "$z-8b8dc93") (Type "ConceptNode"))
  )
  (And
    (Evaluation
      (GroundedPredicate "scm: true-enough")
      (Inheritance (Concept "criminal") (Variable "$B-7e054a97")))
    (Evaluation
      (GroundedPredicate "scm: true-enough")
      (And
        (Evaluation
          (Predicate "sell")
          (List
            (Variable "$who")
            (Variable "$y-281a7166")
            (Variable "$z-8b8dc93")))
        (Inheritance (Variable "$who") (Concept "American"))
        (Inheritance (Variable "$y-281a7166") (Concept "weapon"))
        (Inheritance (Variable "$z-8b8dc93") (Concept "hostile"))))
    (Not (Identical (Concept "criminal") (Variable "$who")))
    (And
      (Evaluation
        (Predicate "sell")
        (List
          (Variable "$who")
          (Variable "$y-281a7166")
          (Variable "$z-8b8dc93")))
      (Inheritance (Variable "$who") (Concept "American"))
      (Inheritance (Variable "$y-281a7166") (Concept "weapon"))
      (Inheritance (Variable "$z-8b8dc93") (Concept "hostile")))
    (Present
      (Inheritance (Variable "$B-7e054a97") (Concept "criminal"))
      (Inheritance (Concept "criminal") (Variable "$B-7e054a97")))
    (Evaluation
      (GroundedPredicate "scm: true-enough")
      (Inheritance (Variable "$B-7e054a97") (Concept "criminal")))
    (Not (Identical (Concept "criminal") (Concept "criminal")))
  )))
