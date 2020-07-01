;
; choice-double.scm
;
; Unit testing for Choices in the pattern matcher.
; Much link choice-embed, except it ahs two choice links
;
(use-modules (opencog))
(use-modules (opencog exec))

;;; Populate the atomspace with four small trees.
(Member (Concept "Tom") (Concept "ways and means"))
(Member (Concept "Joe") (Concept "ways and means"))
(Member (Concept "Hank") (Concept "ways and means"))
(Member (Concept "Dick") (Concept "agriculture"))

;;; the list link serves no purpose other than to "embed"
(List (Member (Concept "Tom") (Concept "Senator")))
(List (Member (Concept "Dick") (Concept "Senator")))
(List (Member (Concept "Joe") (Concept "Representative")))

;; We should NOT find Hank!
(List (Member (Concept "Hank") (Concept "CEO")))

;;; Two clauses; they are both connected with a common variable.
(define double
	(Bind
		(And
			(Choice
				(Member (Variable "$x") (Concept "ways and means"))
				(Member (Variable "$x") (Concept "agriculture")))
			(List
				(Choice
					(Member (Variable "$x") (Concept "Senator"))
					(Member (Variable "$x") (Concept "Representative")))))
		(Variable "$x")))
