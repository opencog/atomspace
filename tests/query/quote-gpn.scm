;
; quote-gpn.scm
;
; Test QuoteLink -- check for appropriate search scoping.
;

; some data
(EvaluationLink
	(GroundedPredicateNode "scm:do_stuff")
	(ListLink
		(ConceptNode "thing-a")
		(ConceptNode "thing-b")
	)
)

; The pattern below can confuse the search start, because
; the first constant link in the clause is the quote ... 
; and that's won't provide the desired start ...
(define bindy
	(BindLink
		(VariableNode "$stuff")
		(EvaluationLink
			(QuoteLink (GroundedPredicateNode "scm:do_stuff"))
			(VariableNode "$stuff")
		)
		(VariableNode "$stuff")
	)
)

; data to check Times matching
(TimesLink
	(NumberNode 3)
	(NumberNode 5)
	)

; Pattern uses QuoteLink to match TimesLink without
; making actual calculations
(define get-times-link
	(GetLink
		(VariableList
			(VariableNode "$a")
			(VariableNode "$b")
			)
		(QuoteLink
			(TimesLink
				(UnquoteLink (VariableNode "$a"))
				(UnquoteLink (VariableNode "$b"))
				)
			)
		)
	)
