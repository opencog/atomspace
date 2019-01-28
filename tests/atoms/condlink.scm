
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
