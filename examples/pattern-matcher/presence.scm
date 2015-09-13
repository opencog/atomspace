;;
;; presence.scm
;;
;; Four different ways to check for the existance of some structure
;; in the atomspace.
;;
;; Below are four different approaches to determining if the atomspace
;; contains the link
;;    (ListLink (AnchorNode "Room State") (ConceptNode "room empty"))
;;
;; If the atomspace does contain the above, then the print-msg
;; function is run. Each different method has its strengths and
;; weaknesses. Several of the methods are good for designing behavior
;; trees that can run in the atomspace.  Others fit the more traditional
;; BindLink AtmSpace query paradigm.

(add-to-load-path "/usr/local/share/opencog/scm")

(use-modules (opencog))
(use-modules (opencog query))
(use-modules (opencog exec))

(load-from-path "utilities.scm")

;; Is the room empty, or is someone in it?
;; One of several different states can be linked to the state variable.
(define room-state (AnchorNode "Room State"))
(define room-empty (ConceptNode "room empty"))
(define room-nonempty (ConceptNode "room nonempty"))

;; Assume room empty at first
(ListLink room-state room-empty)

; Print a message, return a TV value
(define (tv-print-msg)
	(display "Hello, I've been triggered!\n") (stv 1 1))

; Print an atom, return a TV value
(define (tv-print-atom atom)
	(format #t "Hello, I got this atom: ~a\n" atom) (stv 1 1))

; Print an Atom, return an Atom.
(define (atom-print-atom atom)
	(format #t "Hello, Exectuting with atom: ~a\n" atom) atom)

#|
; ------------------------------------------------------
; ------------------------------------------------------

;; This variant uses PresentLink to place the room-state into
;; a variable, and then uses EqualLink to check the value of
;; that variable.
;;
;; This variant haas an advantage over the next one, as it requires
;; only one invocation of the pattern matcher, not two. Because it
;; uses the SequentialAndLink, it is in a form appropriate for creating
;; a behavior tree.
;;
(define empty-sequence
	(SatisfactionLink
		;; SequentialAndLink - verify predicates in sequential order.
		(SequentialAndLink
			;; Assign the room-state to variable $x
			(PresentLink (ListLink room-state (VariableNode "$x")))
			;; If the variable $x equals the emtpry state, then ...
			(EqualLink (VariableNode "$x") room-empty)
			;; ... then print a message.
			(EvaluationLink
				(GroundedPredicateNode "scm: tv-print-atom")
				(ListLink (VariableNode "$x")))
		)))

(cog-satisfy empty-sequence)

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

(define get-empty-seq
	(SatisfactionLink
		;; Perform operations in sequential order.
		(SequentialAndLink
			;; Check for equality ...
			(EqualLink
				(SetLink room-empty)
				;; Retrieve the room state; place it into a SetLink
				(GetLink (ListLink room-state (VariableNode "$x"))))

			;; If the EqualLink evaluated to TRUE, then print the message.
			(EvaluationLink
				(GroundedPredicateNode "scm: tv-print-msg")
				(ListLink))  ; zero arguments passed to function
		)))

(cog-satisfy get-empty-seq)

|#
; ------------------------------------------------------
;; This variant uses the traditional BindLink format to trigger
;; the execuation of a schema.  It is similar to the first example,
;; except for these notable differences:
;;
;; -- The BindLink does not use SequentialAnd, and thus any embedded
;;    GPN may or may not run after the EqualLink; there is no guarantee
;;    of ordered execution.
;; -- The action to be performed must be in the form of a GSN, and thus
;;    it must return an atom, not a truth value.
;; -- Because of the above, actions cannot be chained: this is not
;;    suitable for creating a behavior tree.

(define (print-atom) (display "Hello, I've been triggered!\n") (stv 1 1))

(define bind-empty
	(BindLink
		;; Perform operations in sequential order.
		(AndLink
			;; Assign the room-state to variable $x
			(ListLink room-state (VariableNode "$x"))
			;; If the variable $x equals the emtpry state, then ...
			(EqualLink (VariableNode "$x") room-empty)
		)
		;; If the EqualLink evaluated to TRUE, then print the message.
		(ExecutionOutputLink
				(GroundedSchemaNode "scm: atom-print-atom")
				(ListLink (VariableNode "$x")))
		))

(cog-bind bind-empty)
