;
; boolean-predicate-test.scm -- Test boolean returns from GroundedPredicates
;
; Test that Scheme predicates can return #t and #f
; and that these are properly converted to BoolValue.
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

;; Test that #t converts to BoolValue true
(test-assert "scheme-true-conversion"
   (equal? (cog-execute! eval-scheme-true) (BoolValue #t)))

;; Test that #f converts to BoolValue false
(test-assert "scheme-false-conversion"
   (equal? (cog-execute! eval-scheme-false) (BoolValue #f)))

;; Test that results are actually BoolValues by checking the type
(test-assert "scheme-true-is-bool"
   (eq? (cog-type (cog-execute! eval-scheme-true)) 'BoolValue))

(test-assert "scheme-false-is-bool"
   (eq? (cog-type (cog-execute! eval-scheme-false)) 'BoolValue))

(test-end tname)

(opencog-test-end)
