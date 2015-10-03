;
; choice.scm
;
; A very simple example of using the ChoiceLink.
;

; Populate the atomspace with some data
(EvaluationLink
	(PredicateNode "has-color")
	(ListLink
		(ConceptNode "apple")
		(ConceptNode "green")))

(EvaluationLink
	(PredicateNode "has-color")
	(ListLink
		(ConceptNode "bannana")
		(ConceptNode "yellow")))

(EvaluationLink
	(PredicateNode "has-color")
	(ListLink
		(ConceptNode "strawberry")
		(ConceptNode "red")))

; Look for fruit that is red or green.
; This won't work; although it looks nice, the ChoiceLink cannot
; be used in this way. Sorry.
(define find-fruit
	(GetLink
		(EvaluationLink
			(PredicateNode "has-color")
			(ListLink
				(VariableNode "$fruit")
				(ChoiceLink
					(ConceptNode "red")
					(ConceptNode "green"))))))


; This will work; the ChoiceLink appears at the top.
(define find-fruit
	(GetLink
		(ChoiceLink
			(EvaluationLink
				(PredicateNode "has-color")
				(ListLink
					(VariableNode "$fruit")
					(ConceptNode "red")))
			(EvaluationLink
				(PredicateNode "has-color")
				(ListLink
					(VariableNode "$fruit")
					(ConceptNode "green"))))))

; Run the query:
(cog-satisfying-set find-fruit)

; The answer will be what you expect.
