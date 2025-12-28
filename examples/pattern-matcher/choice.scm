;
; choice.scm -- Using the ChoiceLink to explore alternatives.
;
; A very simple example of using the ChoiceLink.
;

; Populate the AtomSpace with some data
(Edge
	(Predicate "has-color")
	(List
		(Concept "apple")
		(Concept "green")))

(Edge
	(Predicate "has-color")
	(List
		(Concept "banana")
		(Concept "yellow")))

(Edge
	(Predicate "has-color")
	(List
		(Concept "strawberry")
		(Concept "red")))

; Look for fruit that is red or green.
; This won't work; although it looks nice, the ChoiceLink cannot
; be used in this way. Sorry.
(define find-fruit
	(Meet
		(Edge
			(Predicate "has-color")
			(List
				(Variable "$fruit")
				(Choice
					(Concept "red")
					(Concept "green"))))))


; This will work; the ChoiceLink appears at the top.
(define find-fruit
	(Meet
		(Choice
			(Edge
				(Predicate "has-color")
				(List
					(Variable "$fruit")
					(Concept "red")))
			(Edge
				(Predicate "has-color")
				(List
					(Variable "$fruit")
					(Concept "green"))))))

; Run the query:
(cog-execute! find-fruit)

; The answer will be what you expect.
