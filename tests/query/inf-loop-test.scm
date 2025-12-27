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
		(TypeOf (DontExec (Variable "$atom"))) ; rewrite
	))

(define all-types (cog-execute! get-all-types))
(format #t "The types: ~A\n" all-types)

; The result is truly weird.
(define all-types-result
	(UnisetValue
		(Type 'Predicate)
		(Type 'Query)
		(Type 'Type)
		(Type 'TypedVariable)
		(Type 'Variable)
		(Type 'UnisetValue)))
; (format #t "The expected result: ~A\n" all-types-result)

(test-assert "query types"
	(equal? all-types all-types-result))

(define all-types-again (cog-execute! get-all-types))
(format #t "The types, again: ~A\n" all-types-again)

(test-end tname)

(opencog-test-end)
