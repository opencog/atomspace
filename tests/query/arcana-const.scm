;
; Unit testing for a constant pattern
;
(use-modules (opencog))
(use-modules (opencog exec))

(define marconi
	(ListLink
		(ConceptNode "Marconi")
		(ConceptNode "developed")
		(ConceptNode "the")
		(ConceptNode "first")
		(ConceptNode "practical")
		(ConceptNode "wireless.")))

; A query with no variables in it.
(define who
	(BindLink
		(ListLink
			(ConceptNode "WHO")
			(ConceptNode "INVENTED")
			(ConceptNode "RADIO"))
		marconi))

(DefineLink (DefinedSchemaNode "Marco did") marconi)

; Same query as above, but with the answer wrapped up in a define.
(define whodfn
	(BindLink
		(ListLink
			(ConceptNode "WHO")
			(ConceptNode "INVENTED")
			(ConceptNode "RADIO"))
		(DefinedSchemaNode "Marco did")))
