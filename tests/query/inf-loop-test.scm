#! /usr/bin/env guile
-s
!#
;
; inf-loop-test.scm -- Verify that inf loops don't happen.
;

(use-modules (opencog))
(use-modules (opencog test-runner))

(opencog-test-runner)
(define tname "inf-loop-test")
(test-begin tname)

(define get-all-queries
	(Query
		(TypedVariable (Variable "$atom") (Type 'Query))
		(Variable "$atom") ; match anything
		(Variable "$atom") ; rewrite
	))

(test-assert "query queries"
	(equal?
		(cog-execute! get-all-queries)
		(UnisetValue get-all-queries)))

(define get-all-types
	(Query
		(Variable "$atom") ; vardecl
		(Variable "$atom") ; match anything
		(TypeOf (Variable "$atom")) ; rewrite
	))

(define all-types (cog-execute! get-all-types))
(format #t "The types: ~A\n" all-types)

(test-end tname)

(opencog-test-end)
