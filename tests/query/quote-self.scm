;
; quote-self.scm
; Unit test for issue #2583
;
(use-modules (opencog) (opencog exec))

; The Glob is undeclared .. and untyped... thus its free to
; match anything, including the implicand.
;
(define self-ground
	(Query
		(Plus (Glob "$op") (Number 10))
		(Quote (Plus (Unquote (Glob "$op")) (Number 10)))))

; The expected result .. this is not entirely obvious because
; it is hard to read, cause it's self-referential. First, the
; glob is grounded by unquote-glob. Then the quote-unquote is
; removed, and the unquote-glob is inserted. Thus we expect
; the below.
(define expect
	(Plus (Unquote (Glob "$op")) (Number 10)))

; (cog-execute! self-ground)
