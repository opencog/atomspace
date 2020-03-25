;
; Test arithmetic links with globs
;
(use-modules (opencog) (opencog exec))

(PlusLink (Number "1") (Number "2") (Number "3") (Number "4"))

(define glob-for-plus
	(BindLink
		(PlusLink
			(Number "1") (Glob "$star") (Number "4"))
		(PlusLink
			(Number "1") (Glob "$star") (Number "4") (Number "5"))))

; (cog-execute! glob-for-plus) should return (SetLink (Number "15"))
