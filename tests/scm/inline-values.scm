;
; inline-values.scm -- Unit test values specified in-line with atom.
;
(use-modules (opencog))
(use-modules (opencog test-runner))

; ---------------------------------------------------------------------
(opencog-test-runner)
(define tname "inline_values")
(test-begin tname)

; Specify an in-line key-value pair.
(Number 42 43 44 (list (cons (Predicate "p") (FloatValue 1 2 3))))

; Verify that it got recorded.
(define nu (Number 42 43 44))
(test-assert "Number Keys" (equal? 1 (length (cog-keys nu))))
(test-assert "Number Num"
	(equal? (cog-value nu (Predicate "p")) (FloatValue 1 2 3)))

; ----------
; Do it again.  Note that Numbers above and Concepts follow
; a different code path.
(Concept "a" (list (cons (Predicate "p") (FloatValue 1 2 3))))

(define ca (Concept "a"))

(test-assert "Concept Keys" (equal? 1 (length (cog-keys ca))))
(test-assert "Concept Num"
	(equal? (cog-value ca (Predicate "p")) (FloatValue 1 2 3)))

; Add another, and modify.
(Concept "a" (list
	(cons (Predicate "p") (FloatValue 11 22 33))
	(cons (Predicate "q") (StringValue "p" "q" "r"))))

(test-assert "Concept Keys" (equal? 2 (length (cog-keys ca))))
(test-assert "Concept Numa"
	(equal? (cog-value ca (Predicate "p")) (FloatValue 11 22 33)))
(test-assert "Concept Str"
	(equal? (cog-value ca (Predicate "q")) (StringValue "p" "q" "r")))

; ----------
; Like above, but for links.
;
(Link (Concept "foo") (Concept "bar")
	(list
		(cons (Predicate "num") (FloatValue 4 5 6))
		(cons (Predicate "str") (StringValue "x" "y" "z"))))

(define rli (Link (Concept "foo") (Concept "bar")))
;;;(test-assert "List Keys" (equal? 2 (length (cog-keys rli))))
;;;(test-assert "List Num"
;;;	(equal? (cog-value rli (Predicate "num")) (FloatValue 4 5 6)))
;;;(test-assert "List Str"
;;;	(equal? (cog-value rli (Predicate "str")) (StringValue "x" "y" "z")))

(test-end tname)
