;
; query-meet.scm -- MeetLink usage example.
;
; The MeetLink and the GetLink are both very similar ...

(use-modules (opencog) (opencog exec))

; -------------
; Create three bits of "knowledge".
(Evaluation
	(Predicate "foobar") (List (Concept "funny") (Concept "thing")))
(Evaluation
	(Predicate "foobar") (List (Concept "funny") (Concept "story")))
(Evaluation
	(Predicate "foobar") (List (Concept "funny") (Concept "joke")))

; -------------
; Define a simple query. It looks for the funny stuff.
(define meet
	(Meet
		(TypedVariable (Variable "$x") (Type 'ConceptNode))
		(Evaluation
			(Predicate "foobar")
			(List (Concept "funny") (Variable "$x")))
	))
