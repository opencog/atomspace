;
; bool-library.scm -- Test assorted BoolValue logic ops
;
; To run by hand, just say `guile -s bool-library.scm`.
;

(use-modules (srfi srfi-1))
(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))

(opencog-test-runner)
(define tname "bool-library-test")
(test-begin tname)

(define foo (Concept "foo"))
(define bar (Concept "bar"))
(define key (Predicate "some key"))
(define kee (Predicate "other key"))

(cog-set-value! foo key (BoolValue 0 1 0 1 0))
(cog-set-value! bar kee (BoolValue 1 1 0 1 1))

; -----------------------------------------------

(test-assert "or-link"
	(equal? (BoolValue 1 1 0 1 1)
		(cog-execute! (BoolOr (BoolValueOf foo key) (BoolValueOf bar kee)))))

(test-assert "and-link"
	(equal? (BoolValue 0 1 0 1 0)
		(cog-execute! (BoolAnd (BoolValueOf foo key) (BoolValueOf bar kee)))))

(test-assert "not-link"
	(equal? (BoolValue 1 0 1 0 1)
		(cog-execute! (BoolNot (BoolValueOf foo key)))))

(test-assert "or-not-link"
	(equal? (BoolValue 1 1 1 1 1)
		(cog-execute!
			(BoolOr
				(BoolValueOf foo key)
				(BoolNot (BoolValueOf foo key))))))

(test-assert "and-not-link"
	(equal? (BoolValue 0 0 0 0 0)
		(cog-execute!
			(BoolAnd
				(BoolValueOf foo key)
				(BoolNot (BoolValueOf foo key))))))

(test-end tname)

(opencog-test-end)
