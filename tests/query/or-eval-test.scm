;
; or-eval-test.scm -- Verify OrLink with GroundedPredicates in it
; Tests for problem reported in opencog/atomspace#2931

(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))

;; Functions
(define tvkey (Predicate "*-TruthValueKey-*"))
(define (tv->bool tv)
  (cond
    ((equal? tv #t) #t)
    ((equal? tv #f) #f)
    (else #f)))
(define (true? A)
  (tv->bool (cog-value A tvkey)))
(define (always-true) #t)

;; KB
(cog-set-value! (Inheritance (Concept "human") (Concept "person"))
  (Predicate "*-TruthValueKey-*") (BoolValue #t))

;; Query
(define query-plain
(CollectionOf (Meet
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
      (Concept "person")))))
)

(define query-present
(CollectionOf (Meet
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
        (Concept "person"))))))
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
