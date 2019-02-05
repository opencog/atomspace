;
; gsn.scm -- Graph queries that trigger actions.
;
; After a pattern-match has been found, arbitrary code can be
; triggered to run.  This demo shows how.
;
; Running arbitrary functions upon match can be useful for any number
; of reasons:  to send a message whenever a match is found; to perform
; some particularly complex or odious computation, and so on.

(use-modules (opencog))

;; Some arbitrary function, taking one atom as an argument.
;; This function could take zero, two or more arguments; however,
;; in general, it should always return an atom.  It doesn't have to;
;; if it returns something else, then that will e discarded and
;; replaced by the invalid atom.
(define (say-hello atom)
	(display "Hello, ")
	(display (cog-name atom))
	(display "!")
	(newline)
	atom
)

;; Executing the below will cause the "say-hello" function to be called,
;; with the list of atoms in the ListLink being the arguments to the
;; function. To execute, do this:
;;
;;    (cog-execute! say-hello-to-linas)
;;
;; The number of atoms in the ListLink should correspond to the number
;; of arguments of the called function. Failure for the two to
;; correspond will generally result in a hang or crash.
(define say-hello-to-linas
	(ExecutionOutput
		(GroundedSchema "scm: say-hello")
			(List (Concept "Linas"))))


;; ExecutionOutputLinks are particularly useful when combined with the
;; pattern matcher. They can be used to invoke a function every time
;; that a match is found.
;;
;; To demonstrate this, we need to populate the AtomSpace with some data.
;; In this case, some assertions about who is human.
(Evaluation
	(Predicate "is-a")
	(List (Concept "Linas") (Concept "human"))
)

(Evaluation
	(Predicate "is-a")
	(List (Concept "Ben") (Concept "human"))
)

;; Define a pattern that searches for everyone who is a human, and then
;; invokes the say-hello function on each match.
(define find-humans
	(Bind
		;; Declare the variable to be grounded.
		(Variable "$person")
		;; The pattern to be searched for.
		(Evaluation
			(Predicate "is-a")
			(List (Variable "$person") (Concept "human")))

		;; The procedure to invoke when a grounding is found.
		(ExecutionOutput
			(GroundedSchema "scm: say-hello")
			(List (Variable "$person")))))

;; The below should cause two hello messages to be printed, when
;; it is run.
;;
(cog-execute! find-humans)
