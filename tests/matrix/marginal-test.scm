
(use-modules (opencog) (opencog test-runner))
(use-modules (opencog matrix))

(opencog-test-runner)

; ---------------------------------------------------------------
(define tmarg "simple marginal-counting test")
(test-begin tmarg)

(define epa (make-evaluation-pair-api
	(Predicate "foo") 'Concept 'Concept
	(Any "leftie") (Any "roost") "marge" "Marginal Test"))

(define sep (add-pair-stars epa))
(define cti (add-count-api sep))
(define mgi (add-marginal-count cti))

(mgi 'pair-inc (Concept "foo") (Concept "bar") 42)

; (test-equal "two-arc l4" l4 lex)
(test-end tmarg)
