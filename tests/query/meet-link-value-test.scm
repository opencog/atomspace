;
; meet-link-value.scm
;
; MeetLink should return LinkValues when there is more then
; one variable to report.
;

(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))

(opencog-test-runner)

(define tname "meet-link-value")
(test-begin tname)

; A pair of vectors
(Evaluation (Predicate "has legs") (Concept "dog") (CountTruthValue 1 0 1))
(Evaluation (Predicate "has nose") (Concept "dog") (CountTruthValue 1 0 2))
(Evaluation (Predicate "has tail") (Concept "dog") (CountTruthValue 1 0 3))
(Evaluation (Predicate "furry")    (Concept "dog") (CountTruthValue 1 0 4))
(Evaluation (Predicate "domestic") (Concept "dog") (CountTruthValue 1 0 5))

(Evaluation (Predicate "has legs") (Concept "cat") (CountTruthValue 1 0 1))
(Evaluation (Predicate "has nose") (Concept "cat") (CountTruthValue 1 0 2))
(Evaluation (Predicate "has tail") (Concept "cat") (CountTruthValue 1 0 3))
(Evaluation (Predicate "furry")    (Concept "cat") (CountTruthValue 1 0 4))
(Evaluation (Predicate "domestic") (Concept "cat") (CountTruthValue 1 0 5))

(define flow-pairs
	(Meet
		(VariableList
			(TypedVariable (Variable "$cpt") (Type 'Concept))
			(TypedVariable (Variable "$prop") (Type 'Predicate)))
		(Present
			(Evaluation (Variable "$prop") (Variable "$cpt")))))

(define qvalue (cog-execute! flow-pairs))

(test-assert "Return value is a container value"
	(cog-subtype? 'ContainerValue (cog-type qvalue)))

(test-assert "Size of queue is ten"
	(equal? 10 (length (cog-value->list qvalue))))

; Verify they are all link-values
(for-each
	(lambda (LV)
		(test-assert "Its a link value"
			(equal? (cog-type LV) 'LinkValue)))
	(cog-value->list qvalue))

; Verify that cog-value-ref works correctly
(for-each
	(lambda (N)
		(define LV (cog-value-ref qvalue N))
		(test-assert "Still a link value"
			(equal? (cog-type LV) 'LinkValue)))
	(iota 10))

(test-end tname)

(opencog-test-end)
