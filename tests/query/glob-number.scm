;
; glob-number.scm
;
; Test arithmetic links with globs
;
(use-modules (opencog) (opencog exec))

; From issue #2528
(PlusLink (Number "1") (Number "2") (Number "3") (Number "4"))

(define glob-for-plus
	(BindLink
		(PlusLink
			(Number "1") (Glob "$star") (Number "4"))
		(PlusLink
			(Number "1") (Glob "$star") (Number "4") (Number "5"))))

; (cog-execute! glob-for-plus) should return (SetLink (Number "15"))

; From issue #2564
(PlusLink (Number 3) (Number 10))

(define glob-for-ten
	(BindLink
		(Plus (Glob "$op") (Number 10))
		(Times (Plus (Glob "$op") (Number 10)) (Number 30))))

; (cog-execute! glob-for-ten) should return (SetLink (Number "390"))
