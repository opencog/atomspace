;
; Unit testing for absence of multiple terms
;
(use-modules (opencog))
(use-modules (opencog query))

; (load-from-path "utilities.scm")

; Clause to match during query - are Agents Mulder and Scully around?
(define mulder
	(EvaluationLink
		(PredicateNode "Agent Mulder")
		(ListLink (VariableNode "$x"))))

(define scully
	(EvaluationLink
		(PredicateNode "Agent Scully")
		(ListLink (VariableNode "$x"))))

; Call in the agents
(define call-mulder
	(InsertLink
		(TypeNode "EvaluationLink")
		(PredicateNode "Agent Mulder")
		(ListLink (ConceptNode "Exploring Area 51"))))

(define call-scully
	(InsertLink
		(TypeNode "EvaluationLink")
		(PredicateNode "Agent Scully")
		(ListLink (ConceptNode "Late night in the morgue"))))

; Make the agents go away.
(define discredit-mulder
	(BindLink (ImplicationLink mulder (DeleteLink mulder))))

(define discredit-scully
	(BindLink (ImplicationLink scully (DeleteLink scully))))

; Status of the UFO
(define ufo-state (AnchorNode "UFO"))
(define ufo-denied (ConceptNode "Government denies knowledge"))
(define ufo-exists (ConceptNode "Located at Area 51"))

; Initial state: UFO exists
(ListLink ufo-state ufo-exists)

; The UFO exists only if both Mulder and Scully are not around.
(define is-visible
	(BindLink
		(ImplicationLink
			(AndLink (AbsentLink mulder) (AbsentLink scully))
			(AssignLink (TypeNode "ListLink") ufo-state ufo-exists)
		)
	)
)

; The UFO is denied if either Mulder or Scully are around.
(define is-invisible
	(BindLink
		(ImplicationLink
			(ChoiceLink mulder scully)
			(AssignLink (TypeNode "ListLink") ufo-state ufo-denied)
		)
	)
)

;; Display the current UFO state
(define (show-ufo-state)
   (car (cog-chase-link 'ListLink 'ConceptNode ufo-state)))
