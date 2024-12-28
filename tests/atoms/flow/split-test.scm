;
; split-test.scm -- Verify that SplitLin works.
;

(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))

(opencog-test-runner)
(define tname "split-test")
(test-begin tname)

; -------------------------------------------------------------
; Base function: split on whitespace.

(cog-set-value! (Anchor "rock") (Predicate "blab")
	(StringValue "this is a test" "and so is this"))

(define splitter
	(Split (ValueOf (Anchor "rock") (Predicate "blab"))))

(define words (cog-execute! splitter))
(format #t "Split into words: ~A" words)

(define expected
	(LinkValue
		(StringValue "this")
		(StringValue "is")
		(StringValue "a")
		(StringValue "test")
		(StringValue "and")
		(StringValue "so")
		(StringValue "is")
		(StringValue "this")))

(test-assert "word-list" (equal? words expected))

; -------------------------------------------------------------
; Eat excess whitespace.

(cog-set-value! (Anchor "rock") (Predicate "blab")
	(StringValue "  this  \t\t   is  a  test  " " and so  \r\n\t  is this  "))

(define words (cog-execute! splitter))
(format #t "Excess whitespace words: ~A" words)

(test-assert "eat-list" (equal? words expected))

; -------------------------------------------------------------
; Like above, but for a node

(define splatter
	(Split (Concept "  this  \t\t   is  a  test  and so  \r\n\t  is this  ")))

(define words (cog-execute! splatter))
(format #t "Splatter into words: ~A" words)

(define expected
	(LinkValue
		(Concept "this")
		(Concept "is")
		(Concept "a")
		(Concept "test")
		(Concept "and")
		(Concept "so")
		(Concept "is")
		(Concept "this")))

(test-assert "splatter-list" (equal? words expected))

; -------------------------------------------------------------
; Like above, but specifying the output type.

(define splutter
	(Split
		(Type 'ListLink)
		(Concept "  this  \t\t   is  a  test  and so  \r\n\t  is this  ")))

(define words (cog-execute! splutter))
(format #t "Splutter into words: ~A" words)

(define expected
	(ListLink
		(Concept "this")
		(Concept "is")
		(Concept "a")
		(Concept "test")
		(Concept "and")
		(Concept "so")
		(Concept "is")
		(Concept "this")))

(test-assert "splutter-list" (equal? words expected))

; -------------------------------------------------------------
(test-end tname)

(opencog-test-end)
