;
; Test for infinite loop caused in #2357 fixed in #2360
; The stuff in here is kind of crazy, but all if it is needed
; to provoke the bug.

(use-modules (opencog) (opencog exec) (opencog logger))
; (cog-logger-set-level! "fine")
; (cog-logger-set-stdout! #t)

; Target graph to be found
(ListLink
	(LambdaLink (VariableList (VariableNode "$W"))
		(PresentLink
			(InheritanceLink (VariableNode "$W") (Concept "A"))
			(InheritanceLink (Concept "A") (Concept "B"))))
	(NumberNode "5.000000"))

; This is never matched, but needs to be present to trigger
; several explores in `do_term_up()`
(AndLink
	(InheritanceLink (VariableNode "$W") (Concept "A"))
	(InheritanceLink (Concept "A") (Concept "B")))

; Query to run.
; The Quote...Unquote is needed to trigger the bug; removing
; the quotes hides the bug.  Note that in the original bug report,
; the OrderedLink was a LambdaLink. OK, I put it back to Lambda.
; If testing w/o the quotes, use Ordered...
(define query
	(GetLink (PresentLink

		; Matches the target graph
		(ListLink (QuoteLink
			(LambdaLink (UnquoteLink (VariableNode "$f-vardecl"))
				(PresentLink
					(UnquoteLink (VariableNode "$cnj-bodies-1"))
					(UnquoteLink (VariableNode "$cnj-bodies-0")))))
			(VariableNode "$ms-0"))

		; Also matches the target graph
		(ListLink (QuoteLink
			(LambdaLink (UnquoteLink (VariableNode "$f-vardecl"))
				(PresentLink
					(UnquoteLink (VariableNode "$cnj-bodies-1"))
					(UnquoteLink (VariableNode "$cnj-bodies-0")))))
			(VariableNode "$ms-1")))))

; Run the query.
; (cog-execute! query)
