
(use-modules (opencog) (opencog exec))

(define single
	(CondLink
	 (GreaterThanLink
		(PlusLink
		 (NumberNode 2)
		 (NumberNode 2))
		(TimesLink
		 (NumberNode 3)
		 (NumberNode 2)))
	 (PlusLink (Number 3)(Number 2))
	 (FalseLink )
	 (PlusLink (Number 1)(Number 2))
	 (PlusLink (Number 1)(Number -2)))
)

(define nondefault
	(CondLink
   ; If this evaluates to true then the next expresion will execute.
		(GreaterThanLink
		 (PlusLink
			(NumberNode 2)
			(NumberNode 2))
		 (TimesLink
			(NumberNode 1)
			(NumberNode 2)))
		(PlusLink (Number 3)(Number 2))
		(FalseLink )
		(PlusLink (Number 1)(Number 2)))
)

(define listwrapped
 (CondLink
	(ListLink
	 (GreaterThanLink
		(NumberNode 3)
		(NumberNode 4))
	 (NumberNode 0))
	(ListLink
	 (GreaterThanLink
		(NumberNode 4)
		(NumberNode 3))
	 (NumberNode 1))
	(ListLink
	 (TrueLink )
	 (NumberNode 2)))
)