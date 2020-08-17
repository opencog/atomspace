;
; threaded.scm
;
; Part of the unit test for ExecuteThreadedLink.
;
(use-modules (opencog) (opencog exec))

(Inheritance (Concept "rock") (Concept "mineral"))
(Inheritance (Concept "flower") (Concept "plant"))
(Inheritance (Concept "cat") (Concept "animal"))

; Just two threads.
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

; one-hundred things to do in two threads.
(define pmany
	(ExecuteThreaded
		(Number 2)
		(Set
			(map
				(lambda (n)
					(Meet
						(TypedVariable (Variable "X") (Type 'Concept))
						(Inheritance (Variable "X") (Concept "mineral"))))
				(iota 50))
			(map
				(lambda (n)
					(Meet
						(TypedVariable (Variable "X") (Type 'Concept))
						(Inheritance (Variable "X") (Concept "plant"))))
				(iota 50))
	)))

; (cog-execute! pmany)
