;
; gpn.scm -- GroundedPredicateNode Demo
;
; During pattern matching, it can sometimes be useful to call a
; special-purpose function to decide if a given grounding is correct
; or not.  This can be done using the GroundedPredicateNode, as shown
; in this demo.
;
(use-modules (opencog))

; Define a function that takes an atom and returns an OpenCog truth
; value. In this case, it randomly returns true or false, about half the
; time.  The function can take zero, two or more arguments, but it must
; always return an opencog truth value, as the truth value will be used
; during pattern matching to make a grounding decision.
(define (rand-ok atom)
	(define r (random 2))
	(simple-format #t "Random number: ~A " r)
	(if (< 0 r)
		(begin
			(simple-format #t "Picked ~A\n" (cog-name atom))
			(stv 1 1) ; return true
		)
		(begin
			(simple-format #t "Did not pick ~A\n" (cog-name atom))
			(stv 0 1) ; return false
		)
	)
)

; The function can be invoked directly, using the cog-evaluate!
; function. The below defines an Evaluation that, when evaluated,
; sometimes picks something, and sometimes doesn't.
;
(define sometimes
	(Evaluation
		(GroundedPredicate "scm: rand-ok")
		(List (Concept "something"))))

; Try it!  run the following a few times:
; (cog-evaluate! sometimes)

; The pattern-matching requires some data in the AtomSpace to match
; against. So populate the AtomSpace with some data.
(Evaluation
	(Predicate "is-a")
	(List (Concept "Aristotle") (Concept "logician")))

(Evaluation
	(Predicate "is-a")
	(List (Concept "CS Pierce") (Concept "logician")))

;; The following pattern will search for all logicians in the AtomSpace,
;; and then will randomly select some of them, with a 50-50 chance each
;; time. The proposed grounding, made in the first clause of the
;; pattern, is randomly approved of or rejected by the second clause.
(define find-logicians
	(Bind
		; Define the variable to be grounded
		(Variable "$person")

		; Define a list of two clauses, both of which must be satisfied
		(And
			; The first clause: find a grounding for the variable, such
			; that the variable is grounded by the name of a logician.
			(Evaluation
				(Predicate "is-a")
				(List (Variable "$person") (Concept "logician")))

			; The second clause: the proposed grounding, from above,
			; is randomly accepted or rejected.  Several of these can be
			; combined using AndLink, OrLink and NotLink.
			(Evaluation
				(GroundedPredicate "scm: rand-ok")
				(List (Variable "$person"))))

		; Return the grounding, if selected.
		(Variable "$person")))

; Running the below multiple times will return different sets of
; selected logicians each time.
(cog-execute! find-logicians)
(cog-execute! find-logicians)
(cog-execute! find-logicians)
