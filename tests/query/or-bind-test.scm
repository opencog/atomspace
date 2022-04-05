;
; or-bind.scm -- Verify that OrLink works in BindLink, too.
; Fixes Bug opencog/atomspace#2918
;

(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))

;;; (use-modules (opencog logger))
;;; (cog-logger-set-stdout! #t)
;;; (cog-logger-set-level! "FINE")

(opencog-test-runner)
(define tname "or-link-bind-link-test")
(test-begin tname)

; John attends college
(Evaluation
	(Predicate "Attends college")
	(List
		(Concept "John")))

(define get-student
	(Get
		; If X attends school or X attends college
		(Or
			(Evaluation
				(Predicate "Attends school")
				(List
					(Variable "$X")))

			(Evaluation
				(Predicate "Attends college")
				(List
					(Variable "$X"))))))

(test-assert "get John"
	(equal?  (cog-execute! get-student) (Set (Concept "John"))))

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

(test-assert "John is student"
	(equal?  (cog-execute! rewrite-student) (Set
		(Evaluation
			(Predicate "Is student")
			(List (Concept "John"))))))

(test-end tname)

(opencog-test-end)
