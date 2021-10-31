;
; sexpr-query.scm - Store and query s-expressions.
;
; This demo shows how s-expressions can be stored in the AtomSpace,
; and to be searched for.
;
(use-modules (opencog))
(use-modules (opencog exec))

; Store an s-expression in the AtomSpace
; It is passed in as a string.
(SexprAst "(gunk (junk) stunk)")

; Store a second s-expression. This one as a quoted scheme list.
(SexprAst (quote (ork ("asdf") stunk)))

; And a third one, just a pair.
(SexprAst (quote (some stunk)))

; At the risk of becoming very confused, print the contents of the
; AtomSpace.
(cog-prt-atomspace)

; Search for all s-expressions.
(define qry-all
	(Meet (TypedVariable (Variable "$x") (Type 'SexprAst))
		(Present
			(Variable "$x"))))

(cog-execute! qry-all)

; Search for lists containing 'stunk in the last position.
(define qry-list
	(Meet (TypedVariable (Glob "$x") (Type 'SexprAst))
		(Present
			(SexprAst (Glob "$x") (SexprAst 'stunk)))))

(cog-execute! qry-list)

; Search for s-expressions containing only two items, the second
; of which is 'stunk.
(define qry-pair
	(Meet (TypedVariable (Variable "$x") (Type 'SexprAst))
		(Present
			(SexprAst (Variable "$x") (SexprAst 'stunk)))))

(cog-execute! qry-pair)

; That's all folks! The End.
