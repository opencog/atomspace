;
; threaded.scm -- Multi-threading in Atomese
;
; Many atomspace operatins are highly repetitive, and it would be
; nice to parallelize them, so that they can be done at the same time.
; The ExecuteThreadedLink does exactly this -- it wraps executable
; link types, and runs them in distinct threads, then packages up the
; results into a single returned LinkValue.
;
; See also `parallel.scm`, which does something similar, but is more
; appropriate for scripting control.
;
(use-modules (opencog) (opencog exec))

; Populate the AtomSpace with some data.
(Inheritance (Concept "rock") (Concept "mineral"))
(Inheritance (Concept "flower") (Concept "plant"))
(Inheritance (Concept "cat") (Concept "animal"))

; Search the above, in two threads.
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

; Try it!
(cog-execute! pexec)

; The number of threads can be controlled. The below defines
; ten things to do, but does them in just three threads. (To keep
; the example simple, this just repeats each of the searches
; above five times.) Although three threads are used, ten results
; are returned.
(define pmany
	(ExecuteThreaded
		(Number 3)
		(Set
			(map
				(lambda (n)
					(Meet
						(TypedVariable (Variable "X") (Type 'Concept))
						(Inheritance (Variable "X") (Concept "mineral"))))
				(iota 5))
			(map
				(lambda (n)
					(Meet
						(TypedVariable (Variable "X") (Type 'Concept))
						(Inheritance (Variable "X") (Concept "plant"))))
				(iota 5))
	)))

; Try it!
(cog-execute! pmany)
