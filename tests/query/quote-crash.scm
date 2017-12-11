
(use-modules (opencog))
(use-modules (opencog exec))

; This is an infinite loop, except that the QuoteLink is supposed
; to stop the recursion.
(define crasher
	(BindLink
		(VariableNode "$x")
		(ListLink
			(ConceptNode "And the answer is ...")
			(QuoteLink (VariableNode "$x")))))

;; This is an infinite loop, because there are no type restrictions on
;; the variable, and the instantiator can get confused.
(define infloop
	(BindLink
		(VariableNode "$x")
		(ListLink
			(ConceptNode "And the answer is ...")
			(VariableNode "$x"))))
