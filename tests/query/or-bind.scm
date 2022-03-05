;
; Bug #2918
;

(use-modules (opencog) (opencog exec))

; John attends college
(Evaluation
	(Predicate "Attends college")
	(List
		(Concept "John")))

(define rewrite-student
	(Bind
		; If X attends school or X attends college
		(Or
			(Evaluation
				(Predicate "Attends school")
				(List
					(Variable "$X")))

			(Evaluation
				(Predicate "Attends college")
				(List
					(Variable "$X"))))

		; Then X is a student
		(Evaluation
			(Predicate "Is student")
			(List
				(Variable "$X")))))

; (cog-execute! rewrite-student)
