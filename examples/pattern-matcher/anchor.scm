;
; anchor.scm - Obtaining search results incrementally.
;
; Both GetLink and BindLink return all search results wrapped in
; a SetLink. This can be inconvenient in several ways. First,
; in most typical uses, the contents of the SetLink are examined,
; and the set itself is promptly discarded. If it is not discarded,
; then one risks that it just hangs out in the AtomSpace, using up
; storage, but otherwise forgetten and useless.  Another downside
; is that it is impossible to report any results, until all of them
; are found. This can be a problem for extremely long-running searches.
;
; To avoid both of the above issues, one can specify a "drop-off
; location" for the searches; as results arrive, they are attached
; to these drop-off points with MemberLinks, and other parts of the
; system can then grab results from there, and continue. (There are
; also proposals for a promise/future-type mechanis in the same vein,
; but it has not been implemented yet.)
;
; This example shows how to declare a drop-off point. Its actually
; almost trivial.
;
(use-modules (opencog) (opencog exec))

; Some data that we will query over.
(Evaluation (Predicate "foo") (List (Concept "A") (Concept "alpha")))
(Evaluation (Predicate "foo") (List (Concept "B") (Concept "alpha")))
(Evaluation (Predicate "foo") (List (Concept "C") (Concept "alpha")))

; ----------------------------------
; Define a search query. Just an ordinary GetLink - with one twist:
; there is an AnchorNode in the variable declaration.  This AnchorNode
; will be used as the drop-off point.
(define get-link
	(Get
		(VariableList
			(TypedVariable (Variable "$x") (Type 'ConceptNode))
			(Anchor "get-results"))
		(Present
			(Evaluation (Predicate "foo")
				(List (Variable "$x") (Concept "alpha"))))))

; Perform the query. This will return the Anchor, instead of a SetLink.
; (cog-execute! get-link)

; Verify that the expected results showed up. They will be attached
; to the AnchorNode, with MemberLinks.
; (cog-incoming-by-type (AnchorNode "get-results") 'MemberLink)
; ----------------------------------

; Very nearly identical to the above, this shows that the BindLink
; can be used in a similar fashion.
(define bind-link
	(Bind
		(VariableList
			(TypedVariable (Variable "$z") (Type 'ConceptNode))
			(Anchor "bind-results"))
		(Present
			(Evaluation (Predicate "foo")
				(List (Variable "$z") (Concept "alpha"))))
		(Inheritance (Variable "$z") (Concept "letters"))))

; As above: perform the query, and verify that the results showed up.
; (cog-execute! bind-link)
; (cog-incoming-by-type (AnchorNode "bind-results") 'MemberLink)
; ----------------------------------
