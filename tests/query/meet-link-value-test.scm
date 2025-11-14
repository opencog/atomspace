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

(define tvkey (Predicate "*-TruthValueKey-*"))

; A pair of vectors
(cog-set-value! (Edge (Predicate "has legs") (Concept "dog")) tvkey (FloatValue 1 0 1))
(cog-set-value! (Edge (Predicate "has nose") (Concept "dog")) tvkey (FloatValue 1 0 2))
(cog-set-value! (Edge (Predicate "has tail") (Concept "dog")) tvkey (FloatValue 1 0 3))
(cog-set-value! (Edge (Predicate "furry")    (Concept "dog")) tvkey (FloatValue 1 0 4))
(cog-set-value! (Edge (Predicate "domestic") (Concept "dog")) tvkey (FloatValue 1 0 5))

(cog-set-value! (Edge (Predicate "has legs") (Concept "cat")) tvkey (FloatValue 1 0 1))
(cog-set-value! (Edge (Predicate "has nose") (Concept "cat")) tvkey (FloatValue 1 0 2))
(cog-set-value! (Edge (Predicate "has tail") (Concept "cat")) tvkey (FloatValue 1 0 3))
(cog-set-value! (Edge (Predicate "furry")    (Concept "cat")) tvkey (FloatValue 1 0 4))
(cog-set-value! (Edge (Predicate "domestic") (Concept "cat")) tvkey (FloatValue 1 0 5))

(define flow-pairs
	(Meet
		(VariableList
			(TypedVariable (Variable "$cpt") (Type 'Concept))
			(TypedVariable (Variable "$prop") (Type 'Predicate)))
		(Present
			(Edge (Variable "$prop") (Variable "$cpt")))))

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
