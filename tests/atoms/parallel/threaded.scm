;
; threaded.scm
;
; Part of the unit test for ExecuteThreadedLink.
;
(use-modules (opencog) (opencog exec))

(Inheritance (Concept "rock") (Concept "mineral"))
(Inheritance (Concept "flower") (Concept "plant"))
(Inheritance (Concept "cat") (Concept "animal"))

(define pexec
	(ExecuteThreaded
		(Set
			(Meet
				(TypedVariable (Variable "X") (Type 'Concept))
				(Inheritance (Variable "X") (Concept "mineral")))
			(Meet
				(TypedVariable (Variable "X") (Type 'Concept))
				(Inheritance (Variable "X") (Concept "plant"))))
	))

; (cog-execute! pexec)
