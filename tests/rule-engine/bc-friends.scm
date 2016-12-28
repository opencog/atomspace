;; KB for testing BC implication instantiation rule

;; are-friends is symmetric
(ImplicationScope (stv 1 1)
   (VariableList
      (TypedVariable
         (Variable "$X")
         (Type "ConceptNode"))
      (TypedVariable
         (Variable "$Y")
         (Type "ConceptNode")))
   (Evaluation
      (Predicate "are-friends")
      (List
         (Variable "$X")
         (Variable "$Y")))
   (Evaluation
      (Predicate "are-friends")
      (List
         (Variable "$Y")
         (Variable "$X"))))

;; are-friends over musicians is transitive
(ImplicationScope (stv 1 1)
   (VariableList
      (TypedVariable
         (Variable "$X")
         (Type "ConceptNode"))
      (TypedVariable
         (Variable "$Y")
         (Type "ConceptNode"))
      (TypedVariable
         (Variable "$Z")
         (Type "ConceptNode")))
   (And
      (Evaluation
         (Predicate "are-friends")
         (List
            (Variable "$X")
            (Variable "$Y")))
      (Evaluation
         (Predicate "are-friends")
         (List
            (Variable "$Y")
            (Variable "$Z")))
      (Evaluation
         (Predicate "is-musician")
         (Variable "$X"))
      (Evaluation
         (Predicate "is-musician")
         (Variable "$Y"))
      (Evaluation
         (Predicate "is-musician")
         (Variable "$Z")))
   (Evaluation
      (Predicate "are-friends")
      (List
         (Variable "$X")
         (Variable "$Z"))))

;; Facts
(Evaluation (stv 1 1)
   (Predicate "are-friends")
   (List
      (Concept "John")
      (Concept "Mary")))

(Evaluation (stv 1 1)
   (Predicate "are-friends")
   (List
      (Concept "Mary")
      (Concept "Edward")))

(Evaluation (stv 1 1)
   (Predicate "are-friends")
   (List
      (Concept "Michel")
      (Concept "Edward")))

(Evaluation (stv 1 1)
   (Predicate "are-friends")
   (List
      (Concept "Cyril")
      (Concept "John")))

(Evaluation (stv 1 1)
   (Predicate "is-musician")
   (Concept "John"))

(Evaluation (stv 1 1)
   (Predicate "is-musician")
   (Concept "Mary"))

(Evaluation (stv 1 1)
   (Predicate "is-musician")
   (Concept "Edward"))

;; ;; Cheaping facts (useful for debugging)
;; (Evaluation (stv 1 1)
;;    (Predicate "are-friends")
;;    (List
;;       (Concept "Edward")
;;       (Concept "Mary")))
;; (Evaluation (stv 1 1)
;;    (Predicate "are-friends")
;;    (List
;;       (Concept "Mary")
;;       (Concept "John")))
