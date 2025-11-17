;
; or-eval-test.scm -- Verify OrLink with GroundedPredicates in it
; Tests for problem reported in opencog/atomspace#2931
;
; This test is kind of bizarre. The OrLink has one term that
; is always true, so it will always pass. The second term
; tries to look up a TruthValue, but one is never even set,
; so it will always return false, because .. there's no truth
; value. So that's funky.
;
; However, this does expose a certain weirdness: While running,
; this test creates an EdgeLink in a scratch space. And that's
; fine -- that is what scratch spaces are for. But if scratch
; spaces get mis-handled, this test bombs out. So, although it
; is superficially bizarre, it is testing a bug that actually
; shows up.

(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))

;; Functions
(define tvkey (Predicate "*-TruthValueKey-*"))

(define (tv->bool tv)
  (cond
    ((equal? tv (BoolValue #t)) #t)
    ((equal? tv (BoolValue #f)) #f)
    (else #f)))

(define (true? A)
  (tv->bool (cog-value A tvkey)))

(define (always-true) #t)

;; KB
(cog-set-value!
  (Inheritance (Concept "human") (Concept "person"))
  tvkey (BoolValue #t))

;; Query
(define query-plain
(CollectionOf (Meet
  (TypedVariable (Variable "$A") (Type "ConceptNode"))
  (And
    (Or
      (Evaluation
        (GroundedPredicate "scm: true?")
        (Edge
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
        (Edge
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
