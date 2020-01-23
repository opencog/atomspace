;
; absent.scm -- Demo illustrating use of AbsentLink, StateLink
;
; Repeatedly create and destroy an EvaluationLink. Then test to see if
; the EvaluationLink is present in the AtomSpace. If it is, then set a
; state atom that indicates whether it is present or not.
;
; The state is managed using the StateLink, which provides a device
; that "sets state" in the AtomSpace.  It does not actually set state,
; in the sense of changing some atom; this is impossible (by design).
; Instead, it just adds and removes links; the net effect is as if the
; linked atom was a state variable; as its linkage changes, it looks
; as if it's state is changing.
;
; The presence or absence of an atom in the AtomSpace can be determined
; by using the AbsentLink. When evaluated by the pattern matcher, the
; AbsentLink evaluates to "true" only when the atom that it wraps is
; absent from the AtomSpace.  This makes the AbsentLink a rather
; strange link type; roughly speaking, it can be thought of as
; implementing the concept of "reductio ad absurdum" (RAA) or the "law
; of the excluded middle" (LEM) for the AtomSpace.  The AtomSpace can
; be thought as a Kripke frame: it holds all knowledge that is known,
; up to this point in time. The AbsentLink provides a way of checking
; to see if something is currently unknown.  That is, in classical
; logic, everything is assumed to be knowable, and that which is not
; true must be false; this is the law of the excluded middle. By
; contrast, the AtomSpace only contains those sentences known to be
; either true or false (according to their TruthValue); it implements
; a form in Intuitionist Logic.  The AbsentLink is used to determine
; those things that are not yet known.
;
; ----
;
; The state atom is called "Room State", and it will be linked either to
; (ConceptNode "empty") or to (ConceptNode "full"). The (show-room-state)
; function will display the current state, when called.
;
; The room state is set by invoking two patterns: is-visible and
; is-invisible.  The first checks for the presence of the EvaluationLink
; and, if found, sets the room state to full.  The second checks for it's
; absence, and if it is absent, sets the state to "empty".
;
; The EvaluationLink is created and destroyed by running one of two
; patterns, `create` or `destroy`.  The first one uses a `golem`, an
; PutLink that will create the actual EvaluationLink when it is
; executed.  That is, the PutLink defines a potential link, one that
; is not yet in the AtomSpace, but whose description is. When it is
; triggered, the description is turned into the actual link.
;
(use-modules (opencog) (opencog exec))

; Clause to match during query.  This is the EvaluationLink whose
; presence or absence we will be testing for.
(define query
	(Evaluation
		(Predicate "visibility")
		(List (Variable "$x"))))

; Create a golem; the golem is brought to life when its executed.
; i.e. this creates the EvaluationLink when it is executed.
(define golem
	(Put query (Concept "item 42")))

; If an item is visible, delete it, kill it.
(define destroy
	(Bind query (DeleteLink query))
)

; If nothing is visible, then hallucinate the golem into existence.
(define create
	(Bind (Absent query) golem)
)

; The state variable, and it's two states.
(define room-state (Anchor "Room State"))
(define room-empty (Concept "room empty"))
(define room-nonempty (Concept "room nonempty"))

; Initial state: room is empty.
(State room-state room-empty)

; Set the current state if an item is visible.
(define is-visible
	(Bind
		query
		(Put (State room-state (Variable "$x")) room-nonempty)
	)
)

; This has an absent link in it; the link is assigned only when
; the AtomSpace does not have a visible item.
(define is-invisible
	(Bind
		(Absent query)
		(Put (State room-state (Variable "$x")) room-empty)
	)
)

;; Display the current room state
(define (show-room-state)
   (car (cog-chase-link 'StateLink 'ConceptNode room-state)))

; Now, for the actual demonstration.
; First, verify that the room is empty.
(show-room-state)

; Now, create the EvaluationLink.
(cog-execute! create)

; Set the room state. We check for both visibility and invisibility,
; we don't want to assume either case in advance.
(cog-execute! is-visible)
(cog-execute! is-invisible)

; Show the room state.  The room should be full.
(show-room-state)

; Destroy the EvaluationLink, and set the room state.
(cog-execute! destroy)
(cog-execute! is-visible)
(cog-execute! is-invisible)
(show-room-state)

; Do it again, for good luck.
(cog-execute! create)
(cog-execute! is-visible)
(cog-execute! is-invisible)
(show-room-state)

(cog-execute! destroy)
(cog-execute! is-visible)
(cog-execute! is-invisible)
(show-room-state)
