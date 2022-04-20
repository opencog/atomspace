;; Modules
(use-modules (opencog))
(use-modules (opencog exec))

;; Functions
(define-public (bool->tv b)
  (stv (if b 1 0) 1))
(define-public (tv->bool tv)
  (equal? (stv 1 1) tv))
(define-public (true? A)
  (bool->tv (tv->bool (cog-tv A))))
(define (always-true)
  (stv 1 1))

;; KB
(Inheritance (stv 1 1)
  (Concept "human")
  (Concept "person"))

;; Query
(define query
(Get
  (TypedVariable
    (Variable "$A")
    (Type "ConceptNode"))
  (And
    (Or
      (Evaluation
        (GroundedPredicate "scm: true?")
        (Evaluation
          (Predicate "P")
          (List
            (Concept "dog")
            (Variable "$A"))))
      (Evaluation
        (GroundedPredicate "scm: always-true")
        (List)))
    (Inheritance
      (Variable "$A")
      (Concept "person"))))
)
