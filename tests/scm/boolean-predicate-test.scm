;
; boolean-predicate-test.scm -- Test boolean returns from GroundedPredicates
;
; Test that Scheme predicates can return #t and #f (in addition to TruthValues)
; and that these are properly converted to BoolValue and then to SimpleTruthValue.
;

(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))

;; Scheme predicates that return scheme booleans
(define (return-scheme-true) #t)
(define (return-scheme-false) #f)

;; Traditional predicates returning TruthValues (for comparison)
(define (return-tv-true) (stv 1 1))
(define (return-tv-false) (stv 0 1))

;; Test evaluations with boolean returns
(define eval-scheme-true
  (Evaluation
    (GroundedPredicate "scm: return-scheme-true")
    (List)))

(define eval-scheme-false
  (Evaluation
    (GroundedPredicate "scm: return-scheme-false")
    (List)))

;; Test evaluations with TruthValue returns (for comparison)
(define eval-tv-true
  (Evaluation
    (GroundedPredicate "scm: return-tv-true")
    (List)))

(define eval-tv-false
  (Evaluation
    (GroundedPredicate "scm: return-tv-false")
    (List)))

(opencog-test-runner)
(define tname "boolean-predicate-test")
(test-begin tname)

;; Test that #t converts to (stv 1 1)
(test-assert "scheme-true-conversion"
   (equal? (cog-evaluate! eval-scheme-true) (stv 1 1)))

;; Test that #f converts to (stv 0 1)
(test-assert "scheme-false-conversion"
   (equal? (cog-evaluate! eval-scheme-false) (stv 0 1)))

;; Test that boolean returns match TruthValue returns
(test-assert "scheme-bool-matches-tv-true"
   (equal? (cog-evaluate! eval-scheme-true) (cog-evaluate! eval-tv-true)))

(test-assert "scheme-bool-matches-tv-false"
   (equal? (cog-evaluate! eval-scheme-false) (cog-evaluate! eval-tv-false)))

;; Test that results are actually TruthValues
(test-assert "scheme-true-is-tv"
   (cog-tv? (cog-evaluate! eval-scheme-true)))

(test-assert "scheme-false-is-tv"
   (cog-tv? (cog-evaluate! eval-scheme-false)))

(test-end tname)

(opencog-test-end)
