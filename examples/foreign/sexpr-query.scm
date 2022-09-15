;
; sexpr-query.scm - Store and query s-expressions.
;
; This demo shows how s-expressions can be stored in the AtomSpace,
; and how they can be searched for.
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

; Another pair, and some mixed Atomese
(SexprAst (quote (Mork from ork)))
(Inheritance (Concept "tree-like stuffs") (SexprAst "ork"))

; At the risk of becoming very confused, print the contents of the
; AtomSpace.
(cog-prt-atomspace)

;------------------------------------------------------------------
; ATTENTION: The rest of the demo uses native Atomese interfaces.
; In order for this system to be truly useful, some simpler-to-use
; more elegant search interfaces need to be created. Thus, the demo
; below is for Atomese developers only: more casual users will likely
; find this stuff to be wacky, confusing and hard to use.

; Get all s-expressions in the AtomSpace
(cog-get-atoms 'SexprAst)

; Get all expressions that contain 'ork
(cog-incoming-set (SexprAst 'ork))

; Note that the above also returned the InheritanceLink.
; Lets avoid doing that, by asking only for s-expressions.
(cog-incoming-by-type (SexprAst 'ork) 'SexprAst)

; Search for s-expressions containing only two items, the second
; of which is 'stunk. Return only the first item.
(define qry-pair
	(Meet (TypedVariable (Variable "$x") (Type 'SexprAst))
		(Present
			(SexprAst (Variable "$x") (SexprAst 'stunk)))))

(cog-execute! qry-pair)

; Search for lists containing 'stunk in the last position.
; Return only the items that come before.
(define qry-list
	(Meet (TypedVariable (Glob "$x") (Type 'SexprAst))
		(Present
			(SexprAst (Glob "$x") (SexprAst 'stunk)))))

(cog-execute! qry-list)

; That's all folks! The End.
