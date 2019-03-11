;;
;; presence.scm -- Testing for the presence of an Atom.
;;
;; Four different ways to check for the existence of some structure
;; in the AtomSpace.
;;
;; Each variant checks to see if the AtomSpace contains the link
;;    (ListLink (AnchorNode "Room State") (ConceptNode "room empty"))
;;
;; If the AtomSpace does contain the above, then the print-msg function
;; is run. Each different method has its strengths and weaknesses.
;; Several of the methods are good for designing behavior trees that
;; run inside the AtomSpace.  Others fit the more traditional query
;; paradigm.

(use-modules (opencog) (opencog exec))

;; Is the room empty, or is someone in it?
;; One of several different states can be linked to the state variable.
(define room-state (Anchor "Room State"))
(define room-empty (Concept "room empty"))
(define room-nonempty (Concept "room nonempty"))

;; Assume room empty at first
(List room-state room-empty)

; Print a message, return a TV value
(define (tv-print-msg)
	(display "Hello, I've been triggered!\n") (stv 1 1))

; Print an atom, return a TV value
(define (tv-print-atom atom)
	(format #t "Hello, I got this atom: ~a\n" atom) (stv 1 1))

; Print an Atom, return an Atom.
(define (atom-print-atom atom)
	(format #t "Hello, Executing with atom: ~a\n" atom) atom)

; ------------------------------------------------------
; ------------------------------------------------------

;; This variant uses PresentLink to place the room-state into
;; a variable, and then uses EqualLink to check the value of
;; that variable.
;;
;; This variant has an advantage over the next one, as it requires
;; only one invocation of the pattern matcher, not two. Because it
;; uses the SequentialAndLink, it is in a form appropriate for creating
;; a behavior tree.
;;
;; Note that there may be other atoms linked to the Anchor;
;; it there are, then those other atoms are ignored. This may be
;; an advantage or a disadvantage; the next example demands that
;; there be only one atom linked to the Anchor.
;;
(define empty-sequence
	(Satisfaction
		;; SequentialAndLink - verify predicates in sequential order.
		(SequentialAnd
			;; Assign the room-state to variable $x
			;; PresentLink evaluates to 'true' if the ListLink is found;
			;; processing continues to the next statement ONLY if true
			;; is returned by the PresentLink.
			(Present (List room-state (Variable "$x")))
			;; If the variable $x equals the empty state, then ...
			(Equal (Variable "$x") room-empty)
			;; ... then print a message.
			(Evaluation
				(GroundedPredicate "scm: tv-print-atom")
				(List (Variable "$x")))
		)))

(cog-evaluate! empty-sequence)

; ------------------------------------------------------
;; This variant uses a GetLink to fetch the room-state from the
;; AtomSpace, and then uses EqualLink to see if it is in the desired
;; state. Note that this results in *two* invocations of the pattern
;; matcher; the GetLink being the inner one.  Note also that the
;; GetLink returns it's results in a SetLink, so comparison must
;; use a SetLink as well.
;;
;; In this example, the variable $x is bound by the GetLink, and
;; is thus not available outside of the GetLink.  Thus, the grounding
;; for that variable cannot be given to the print-message routine.
;;
;; Unlike the previous example, this one will explicitly fail if there
;; are other atoms linked to the Anchor.  That is, the equality
;; check is making sure that the SetLink has one and only one element
;; in it, which effectively blocks other anchored atoms.  This may be
;; an advantage, or a disadvantage, depending on the situation.

(define get-empty-seq
	(Satisfaction
		;; Perform operations in sequential order.
		(SequentialAnd
			;; Check for equality ...
			(Equal
				(Set room-empty)
				;; Retrieve the room state; place it into a SetLink
				(Get (List room-state (Variable "$x"))))

			;; If the EqualLink evaluated to TRUE, then print the message.
			(Evaluation
				(GroundedPredicate "scm: tv-print-msg")
				(List))  ; zero arguments passed to function
		)))

(cog-evaluate! get-empty-seq)

; ------------------------------------------------------
;; This variant uses the traditional BindLink format to trigger
;; the execution of a schema.  It is similar to the first example,
;; except for these notable differences:
;;
;; -- The BindLink does not use SequentialAnd, and thus any embedded
;;    GPN may or may not run after the EqualLink; there is no guarantee
;;    of ordered execution.
;; -- The action to be performed must be in the form of a GSN, and thus
;;    it must return an atom, not a truth value.
;; -- Because of the above, actions cannot be chained: this is not
;;    suitable for creating a behavior tree.

(define bind-empty
	(Bind
		;; Perform operations in sequential order.
		(And
			;; Assign the room-state to variable $x
			(List room-state (Variable "$x"))
			;; If the variable $x equals the empty state, then ...
			(Equal (Variable "$x") room-empty)
		)
		;; If the EqualLink evaluated to TRUE, then print the message.
		(ExecutionOutput
				(GroundedSchema "scm: atom-print-atom")
				(List (Variable "$x")))
		))

(cog-execute! bind-empty)

; ------------------------------------------------------
;; This variant uses a PutLink-GetLink combination. It is functionally
;; identical to the BindLink; merely, the order in which the action
;; is done is reversed w.r.t. the test.
;;
(define put-empty-atom
	(Put
		;; Replace the value of $x by whatever the GetLink returns.
		(ExecutionOutput
				(GroundedSchema "scm: atom-print-atom")
				(List (Variable "$x")))
		(Get
			;; The variable $y is automatically bound by the GetLink;
			;; it does not escape the scope of the GetLink.
			(And
				;; Search for presence
				(List room-state (Variable "$y"))
				;; Check for equality ...
				(Equal (Variable "$y") room-empty)))
		))

(cog-execute! put-empty-atom)

; ------------------------------------------------------
