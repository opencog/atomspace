#! /usr/bin/env guile
-s
!#
;
; filter-everything-test.scm -- Filter everything in the AtomSpace.
;

(use-modules (opencog))
(use-modules (opencog test-runner))

(opencog-test-runner)
(define tname "filter-everything-test")
(test-begin tname)

; -----------
(define get-all-atoms
	(Meet
		(Variable "$atom") ; vardecl
		(Variable "$atom") ; match anything
	))

(define mis-id-body
	(Filter
		(Rule
			(Variable "$atom") ; rule body, mis-identified as vardecl
			(TypeOf (DontExec (Variable "$atom"))))
		get-all-atoms))

; It would be best if the above did NOT throw, and understood that
; the accept-everything body was indeed the body. But in the interest
; of expedience, we accept the exception.
(test-assert "Mis-identified body"
	(catch #t
		(lambda () (cog-execute! mis-id-body) #f)
		(lambda (key . args) #t)))

(define present-body
	(Filter
		(Rule
			(Present (Variable "$atom")) ; clearly rule body
			(TypeOf (DontExec (Variable "$atom"))))
		get-all-atoms))

(cog-execute! present-body)

(define vardecl-unconstrained
	(Filter
		(Rule
			(Variable "$atom") ; vardel, unconstained
			(Variable "$atom") ; rule body
			(TypeOf (DontExec (Variable "$atom"))))
		get-all-atoms))

(cog-execute! vardecl-unsoncstrained)

(define vardecl-atoms
	(Filter
		(Rule
			(TypedVariable (Variable "$atom") (Type 'Atom)) ; atom only
			(Variable "$atom") ; rule body
			(TypeOf (DontExec (Variable "$atom"))))
		get-all-atoms))

(cog-execute! vardecl-atoms)

; -----------

(test-end tname)
(opencog-test-end)
