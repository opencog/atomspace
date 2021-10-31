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

; Search for all s-expressions.
(define qry-all
	(Meet (TypedVariable (Variable "$x") (Type 'SexprAst))
		(Present
			(Variable "$x"))))

(cog-execute! qry-all)

; Search for s-expressions containing "stunk"
(define qry-few
	(Meet (TypedVariable (Glob "$x") (Type 'SexprAst))
		(Present
			(SexprAst (Glob "$x") (SexprAst "stunk")))))

(cog-execute! qry-few)

; That's all folks! The End.
