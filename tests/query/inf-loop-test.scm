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

(test-end tname)

(opencog-test-end)
