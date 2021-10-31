;
; sexpr-query-test.scm - Store and query s-expressions.
; Test for the matching `sexpr-query.scm` example demo.
;
(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))

(opencog-test-runner)
(define tname "sexpr-query-test")
(test-begin tname)

(SexprAst "(gunk (junk) stunk)")
(SexprAst (quote (ork ("asdf") stunk)))
(SexprAst (quote (some stunk)))
(SexprAst (quote (Mork from ork)))
(Inheritance (Concept "tree-like stuffs") (SexprAst "ork"))

; Get all s-expressions in the AtomSpace
(test-assert "as-contents" (equal? 18 (length (cog-get-atoms 'SexprAst))))

(test-assert "ork incoming" (equal? 4
	(length (cog-incoming-set (SexprAst 'ork)))))

(test-assert "ork sexpr incoming" (equal? 3
	(length (cog-incoming-by-type (SexprAst 'ork) 'SexprAst))))

; Search for s-expressions containing only two items, the second
; of which is 'stunk. Return only the first item.
(define qry-pair
	(Meet (TypedVariable (Variable "$x") (Type 'SexprAst))
		(Present
			(SexprAst (Variable "$x") (SexprAst 'stunk)))))

(test-assert "pair query" (equal? 3
	(length (cog-value->list (cog-execute! qry-pair)))))

; Search for lists containing 'stunk in the last position.
; Return only the items that come before.
(define qry-list
	(Meet (TypedVariable (Glob "$x") (Type 'SexprAst))
		(Present
			(SexprAst (Glob "$x") (SexprAst 'stunk)))))

(test-assert "list query" (equal? 4
	(length (cog-value->list (cog-execute! qry-list)))))

(test-end tname)
