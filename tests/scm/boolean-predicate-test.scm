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

;; Test evaluations with boolean returns
(define eval-scheme-true
  (Evaluation
    (GroundedPredicate "scm: return-scheme-true")
    (List)))

(define eval-scheme-false
  (Evaluation
    (GroundedPredicate "scm: return-scheme-false")
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

;; Test that results are actually TruthValues
(test-assert "scheme-true-is-tv"
   (cog-tv? (cog-evaluate! eval-scheme-true)))

(test-assert "scheme-false-is-tv"
   (cog-tv? (cog-evaluate! eval-scheme-false)))

(test-end tname)

(opencog-test-end)
