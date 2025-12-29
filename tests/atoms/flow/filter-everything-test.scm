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

(cog-execute! mis-id-body)

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
