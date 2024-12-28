;
; split-test.scm -- Verify that SplitLin works.
;

(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))

(opencog-test-runner)
(define tname "split-test")
(test-begin tname)

; -------------------------------------------------------------
; Base function: static non-executable stuff.

(cog-set-value! (Anchor "rock") (Predicate "blab")
	(StringValue "this is a test" "and so is this"))

(define splitter
	(Split (ValueOf (Anchor "rock") (Predicate "blab"))))

(define words (cog-execute! splitter))
(format #t "Split into words: ~A" words)

(test-assert "word-list"
	(equal? words
		(LinkValue
			(StringValue "this")
			(StringValue "is")
			(StringValue "a")
			(StringValue "test")
			(StringValue "and")
			(StringValue "so")
			(StringValue "is")
			(StringValue "this"))))

; -------------------------------------------------------------
(test-end tname)

(opencog-test-end)
