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

; One-hundred things to do in two threads.
; This is set up so that each Query differs from all the others.
; This avoids issues when the same Query/Meet is run in parallel:
; both instances place results on the same result queue; if one
; instance closes the queue while the other is still adding to it,
; an exception gets thrown and everything goes haywire.
(define pmany
	(ExecuteThreaded
		(Number 2)
		(Set
			(map
				(lambda (n)
					(Query
						(TypedVariable (Variable "X") (Type 'Concept))
						(Inheritance (Variable "X") (Concept "mineral"))
						(List (Number n) (Variable "X"))))
				(iota 50))
			(map
				(lambda (n)
					(Query
						(TypedVariable (Variable "X") (Type 'Concept))
						(Inheritance (Variable "X") (Concept "plant"))
						(List (Number n) (Variable "X"))))
				(iota 50))
	)))

; (cog-execute! pmany)
