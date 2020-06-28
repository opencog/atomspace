;
; nested-and.scm
; Test recursively-nested AndLinks
;
(use-modules (opencog) (opencog exec))

(Evaluation (Predicate "has-a")
	(List (Concept "car") (Concept "battery")))

(Evaluation (Predicate "state")
	(List (Concept "battery") (Concept "dead")))

(define did-call #f)

(define (foo x)
	(format #t "foo: ~A\n" x)
	(set! did-call #t)
	(stv 1 1))

(define nestand
	(Get
		(TypedVariable (Variable "$component") (Type 'Concept))
		(And
			(And
				(Present
					(Evaluation (Predicate "has-a")
						(List (Concept "car") (Variable "$component"))))
				(Evaluation (GroundedPredicate "scm: foo")
					(List (Variable "$component"))))
			(And
				(Present
					(Evaluation (Predicate "state")
						(List (Variable "$component") (Concept "dead"))))))))

; (cog-execute! nestand)
