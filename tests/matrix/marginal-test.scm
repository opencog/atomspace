
(use-modules (opencog) (opencog test-runner))
(use-modules (opencog matrix))

(opencog-test-runner)

; ---------------------------------------------------------------
; Make sure that the `add-marginal-count` works as expected.
; This is a very basic sniff test.

(define tmarg "simple marginal-counting test")
(test-begin tmarg)

; Befine a basic matrix.
(define epa (make-evaluation-pair-api
	(Predicate "foo") 'Concept 'Concept
	(Any "leftie") (Any "roost") "marge" "Marginal Test"))

; Add decorators
(define sep (add-pair-stars epa))
(define cti (add-count-api sep))
(define mgi (add-marginal-count cti))

; Add one atom, verify that the marginals are updated.
(mgi 'pair-inc (Concept "foo") (Concept "bar") 42)
(test-equal "left-marg" 42.0 (cog-count (epa 'left-wildcard (Concept "bar"))))
(test-equal "right-marg" 42.0 (cog-count (epa 'right-wildcard (Concept "foo"))))
(test-equal "tot" 42.0 (cog-count (epa 'wild-wild)))

; Add a few more atoms.
(mgi 'pair-inc (Concept "foo") (Concept "bar2") 0.5)
(mgi 'pair-inc (Concept "foo2") (Concept "bar") 1.5)

; The original pair is untouched.
(test-equal "pair" 42.0 (cog-count (epa 'make-pair (Concept "foo") (Concept "bar"))))

; The marginals should be accumulating.
(test-equal "left-marg" 43.5 (cog-count (epa 'left-wildcard (Concept "bar"))))
(test-equal "right-marg" 42.5 (cog-count (epa 'right-wildcard (Concept "foo"))))
(test-equal "tot" 44.0 (cog-count (epa 'wild-wild)))

(test-end tmarg)
