;
; unordered-quote.scm
;
; Test for infinite loop caused in #2357; hacked around in #2360;
; proper fix in #23real-soon-now.
;
; The root cause of the bug is the funky handling of quotations in
; `PatternMatchEngine::record_grounding()` where the recorded grounding
; does not match up with what the pattern matcher thought it was looking
; for, thus confusing the unordered-link traverser into either exploring
; twice (inf loop), or missing a permutation.
;
; The stuff in here looks kind of crazy, but all of it is needed
; to provoke the bug. Simplifying further makes the bug go away.

(use-modules (opencog) (opencog exec) (opencog logger))
; (cog-logger-set-level! "fine")
; (cog-logger-set-stdout! #t)
; (cog-logger-set-timestamp! #f)

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
