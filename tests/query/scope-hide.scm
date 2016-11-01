;
; Unit test for bug opencog/atomspace/issues/950
;

(use-modules (opencog))
(use-modules (opencog query))

;; The ForAll link is a ScopeLink, and so (VariableNode "$X")
;; is a bound var.
(define forall
	(ForAllLink
		(VariableNode "$X")
		(EvaluationLink (Predicate "P") (VariableNode "$X"))))

;; This has the same variable name as the above -- and so,
;; during pattern matching, this should not be considered to be
;; a self-grounding -- $X should bind to $X just fine.
(define getx (Get (ForAllLink (Variable "$X") (Variable "$B"))))

;; Alpha conversion means that getv should be exactly the same atom
;; as getx.  Note that chronological order matters: this MUST be
;; defined after the above, for this unit test to actually test
;; something valid.
(define getv (Get (ForAllLink (Variable "$V") (Variable "$B"))))
