(Implication (stv 1.0 1.0)
   (TypedVariable
      (Variable "$X")
      (Type "ConceptNode"))
   (And (stv 1.0 1.0)
      (Evaluation
         (Predicate "croaks")
         (Variable "$X"))
      (Evaluation
         (Predicate "eats_flies")
         (Variable "$X")))
   (Inheritance
      (Variable "$X")
      (Concept "Frog")))

(Implication (stv 1.0 1.0)
   (TypedVariable
      (Variable "$X")
      (Type "ConceptNode"))
   (And (stv 1.0 1.0)
      (Evaluation
         (Predicate "chirps")
         (Variable "$X"))
      (Evaluation
         (Predicate "sings")
         (Variable "$X")))
   (Inheritance
      (Variable "$X")
      (Concept "Canary")))

(Implication (stv 1.0 1.0)
   (TypedVariable
      (Variable "$X")
      (Type "ConceptNode"))
   (Inheritance
      (Variable "$X")
      (Concept "Frog"))
   (Inheritance
      (Variable "$X")
      (Concept "green")))

(Implication (stv 1.0 1.0)
   (TypedVariable
      (Variable "$X")
      (Type "ConceptNode"))
   (Inheritance
      (Variable "$X")
      (Concept "Canary"))
   (Inheritance
      (Variable "$X")
      (Concept "yellow")))

;KB
(Evaluation (stv 1.0 1.0)
   (Predicate "croaks")
   (Concept "Fritz"))

(Evaluation (stv 1.0 1.0)
   (Predicate "chirps")
   (Concept "Tweety"))

(Inheritance (stv 1.0 1.0)
   (Concept "Tweety")
   (Concept "yellow"))

(Evaluation (stv 1.0 1.0)
   (Predicate "eats_flies")
   (Concept "Tweety"))

(Evaluation (stv 1.0 1.0)
   (Predicate "eats_flies")
   (Concept "Fritz"))
