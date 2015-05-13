;
; Unit testing for absence
;
;;; Populate the atomspace with two things
(use-modules (opencog))
(use-modules (opencog query))

; Clause to match during query
(define query 
	(EvaluationLink
		(PredicateNode "visiblity")
		(ListLink (VariableNode "$x"))))

; Create a golem; the golem is brought to life when its executed.
(define golem
	(InsertLink
		(TypeNode "EvaluationLink")
		(PredicateNode "visiblity")
		(ListLink (ConceptNode "item 42"))))

; If an item is visible, delete it, kill it.
(define destroy
	(BindLink
		(ImplicationLink query (DeleteLink query))
	)
)

; If nothing is visible, then hallucinate the golem into existance.
(define create
	(BindLink
		(ImplicationLink (AbsentLink query) golem)
	)
)

(define room-state (AnchorNode "Room State"))
(define room-empty (ConceptNode "room empty"))
(define room-nonempty (ConceptNode "room nonempty"))

; Initial state: room is empty
(ListLink room-state room-empty)

; Set the current state if an item is visible.
(define is-visible
	(BindLink
		(ImplicationLink
			query
			(AssignLink (TypeNode "ListLink") room-state room-nonempty)
		)
	)
)

; This has an absent link in it; the link is assigned only when
; the atomspace does not have a visible item.
(define is-invisible
	(BindLink
		(ImplicationLink
			(AbsentLink query)
			(AssignLink (TypeNode "ListLink") room-state room-empty)
		)
	)
)

;; Display the current room state
(define (show-room-state)
   (car (cog-chase-link 'ListLink 'ConceptNode room-state)))
