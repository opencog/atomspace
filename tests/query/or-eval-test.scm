;
; or-eval-test.scm -- Verify OrLink with GroundedPredicates in it
; Tests for problem reported in opencog/atomspace#2931

(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))

;; Functions
(define-public (bool->tv b) (stv (if b 1 0) 1))
(define-public (tv->bool tv) (equal? (stv 1 1) tv))
(define-public (true? A) (bool->tv (tv->bool (cog-tv A))))
(define (always-true) (stv 1 1))

;; KB
(Inheritance (stv 1 1) (Concept "human") (Concept "person"))

;; Query
(define query-plain
(Get
  (TypedVariable (Variable "$A") (Type "ConceptNode"))
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

(define query-present
(Get
  (TypedVariable (Variable "$A") (Type "ConceptNode"))
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
    (Present
      (Inheritance
        (Variable "$A")
        (Concept "person")))))
)

(opencog-test-runner)
(define tname "or-eval-test")
(test-begin tname)

(test-assert "human-plain"
   (equal? (cog-execute! query-plain) (Set (Concept "human"))))

(test-assert "human-present"
   (equal? (cog-execute! query-present) (Set (Concept "human"))))

(test-end tname)

(opencog-test-end)
