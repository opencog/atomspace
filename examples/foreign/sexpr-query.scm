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

; Search for an s-expression
(define qry
	(Meet (TypedVariable (Variable "$x") (Type 'SexprAst))
		(Present
			(Variable "$x"))))

(cog-execute! qry)
