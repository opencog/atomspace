;
; Unit testing for absence of a single term.
;
; Repeatedly create and destroy an EvaluationLink. Then test to see if the
; EvaluationLink is present in the atomspace. If it is, then set a state
; atom that indicates whether it is present or not.
;
; The state atom is called "Room List", and it will be linked either to
; (ConceptNode "empty") or to (ConceptNode "full"). The (show-room-state)
; function will display the current state, when called.
;
; The room state is set by invoking two patterns: is-visible and
; is-invisibile.  The first checks for the presence of the EvaluationLink
; and, if found, sets the room state to full.  The second checks for it's
; absence, and if it is absent, sets the state to "empty".
;
; The EvaluationLink is created and destroyed by running one of two
; patterns, `create` or `destroy`.
;
(use-modules (opencog))
(use-modules (opencog exec))

; Clause to match during query
(define query 
	(EvaluationLink
		(PredicateNode "visibility")
		(ListLink (VariableNode "$x"))))

; Create a golem; the golem is brought to life when its executed.
(define golem
	(PutLink query (ConceptNode "item 42")))

; If an item is visible, delete it, kill it.
(define destroy
	(BindLink query (DeleteLink query))
)

; If nothing is visible, then hallucinate the golem into existence.
(define create
	(BindLink (AbsentLink query) golem)
)

(define room-state (AnchorNode "Room List"))
(define room-empty (ConceptNode "room empty"))
(define room-nonempty (ConceptNode "room nonempty"))

; Initial state: room is empty
(StateLink room-state room-empty)

; Set the current state if an item is visible.
(define is-visible
	(BindLink
		query
		(PutLink (StateLink room-state (VariableNode "$x")) room-nonempty)
	))

; This has an absent link in it; the link is assigned only when
; the atomspace does not have a visible item.
(define is-invisible
	(BindLink
		(AbsentLink query)
		(PutLink (StateLink room-state (VariableNode "$x")) room-empty)
	))

;; Display the current room state
(define (show-room-state)
   (car (cog-chase-link 'StateLink 'ConceptNode room-state)))
