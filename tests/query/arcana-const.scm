;
; Unit testing for a constant pattern
;
(use-modules (opencog))
(use-modules (opencog query))

(define marconi
	(ListLink
		(ConceptNode "Marconi")
		(ConceptNode "developed")
		(ConceptNode "the")
		(ConceptNode "first")
		(ConceptNode "practical")
		(ConceptNode "wireless.")))

(define who
	(BindLink
		(ListLink
			(ConceptNode "WHO")
			(ConceptNode "INVENTED")
			(ConceptNode "RADIO"))
		marconi))
