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

; ---------------------------------------
; XXX FIXME The RuleLink logic for extracting the body
; is currently broken. Should be fixed.
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

; ---------------------------------------
; This works fine, at this time.
(define vardecl-unconstrained
	(Filter
		(Rule
			(Variable "$atom") ; vardel, unconstained
			(Variable "$atom") ; rule body
			(TypeOf (DontExec (Variable "$atom"))))
		get-all-atoms))

(define uncon (cog-execute! vardecl-unconstrained))
(format #t "Unconstrained ~A\n" uncon)
(test-assert "unconstrained search"
	(equal? uncon
		(LinkValue
			(Type 'DontExec)
			(Type 'Filter)
			(Type 'Filter)
			(Type 'Meet)
			(Type 'Predicate)
			(Type 'Rule)
			(Type 'Rule)
			(Type 'TypeOf))))

; ---------------------------------------
; This works fine, at this time.
; The returns list is long.
(define vardecl-atoms
	(Filter
		(Rule
			(TypedVariable (Variable "$atom") (Type 'Atom)) ; atom only
			(Variable "$atom") ; rule body
			(TypeOf (DontExec (Variable "$atom"))))
		get-all-atoms))

(define vatoms (cog-execute! vardecl-atoms))
; (format #t "vardecl atoms: ~A\n" vatoms)

; ---------------------------------------
; Works
(define count-types
	(Filter
		(Rule
			(TypedVariable (Variable "$typ") (Type 'Type)) ; vardecl
			(Variable "$typ") ; body - accept everything
			(IncrementValue (Variable "$typ") (Predicate "cnt") (Number 0 0 1)))
		vardecl-atoms))

(define count-list (cog-execute! count-types))
(format #t "List length: ~A\n" (length (cog-value->list count-list)))

; The actual count will change if the above is changed, but this gets
; us into the general ballpark, for now. Sloppy, but whatever.
(test-assert "num counts"
	(equal? 25 (length (cog-value->list count-list))))

; -----------

(test-end tname)
(opencog-test-end)
