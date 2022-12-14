;
; element-of-test.scm -- Test the ElementOfLink.
;
; To run by hand, just say `guile -s element-of-test.scm`.
;

(use-modules (srfi srfi-1))
(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))

(opencog-test-runner)
(define tname "element-of-test")
(test-begin tname)

(define foo (Concept "foo"))
(define bar (Concept "bar"))
(define key (Predicate "some key"))
(define kee (Predicate "other key"))

(cog-set-value! foo key (FloatValue 1 3))
(cog-set-value! bar kee (Number 0 1 3 4))

(define sent (ListLink
	(Concept "this")
	(Concept "is")
	(Concept "a")
	(Concept "test")
	(Concept "foo")))

(define link-key (Predicate "link key"))

(cog-set-value! foo link-key (LinkValue (cog-outgoing-set sent)))

; -----------------------------------------------

(test-assert "float-index"
	(equal? (ListLink (Concept "is") (Concept "test"))
		(cog-execute! (ElementOf (ValueOf foo key) sent))))

(test-assert "number-index"
	(equal? (ListLink
			(Concept "this") (Concept "is")
			(Concept "test") (Concept "foo"))
		(cog-execute! (ElementOf (ValueOf bar kee) sent))))

; -----------------------------------------------

(test-assert "float-link-index"
	(equal? (LinkValue (Concept "is") (Concept "test"))
		(cog-execute! (ElementOf (ValueOf foo key) (ValueOf foo link-key)))))

(test-assert "number-link-index"
	(equal? (LinkValue
			(Concept "this") (Concept "is")
			(Concept "test") (Concept "foo"))
		(cog-execute! (ElementOf (ValueOf bar kee) (ValueOf foo link-key)))))

(test-end tname)

(opencog-test-end)
