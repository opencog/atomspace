;
; or-link-test.scm -- Verify that OrLink produces sums during search.
; Reflects the discussion in issue opencog/atomspace#2644

(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))

(opencog-test-runner)
(define tname "or-link-test")
(test-begin tname)

; Initial data. The is-true looks at the BooValue directly.
(define tvkey (Predicate "*-TruthValueKey-*"))
(define (is-true x) (BoolValueOf x tvkey))

(State (Concept "you") (Concept "thirsty"))
(State (Concept "me") (Concept "hungry"))
(cog-set-value! (Edge (Predicate "cold") (Concept "me"))
	tvkey (BoolValue #t))
(cog-set-value! (Edge (Predicate "tired") (Concept "her"))
	tvkey (BoolValue #t))

(define qr2
	(CollectionOf (Meet (TypedVariable (Variable "someone") (Type 'Concept))
		(Or
			(Present (State (Variable "someone") (Concept "hungry")))
			(Present (State (Variable "someone") (Concept "thirsty")))))))

(test-assert "hungry or thirsty"
	(equal? (cog-execute! qr2) (Set (Concept "you") (Concept "me"))))

; ------------
; As above, but with EvaulationLink
(define qr4
	(CollectionOf (Meet (TypedVariable (Variable "someone") (Type 'Concept))
		(Or
			(Present (State (Variable "someone") (Concept "thirsty")))
			(And
				(Present (Edge (Predicate "cold") (Variable "someone")))
				(is-true (Edge (Predicate "cold") (Variable "someone"))))))))


(test-assert "thirsty or cold"
	(equal? (cog-execute! qr4) (Set (Concept "you") (Concept "me"))))

; ------------
; Same as above, but with implicit PresentLink
(define qr5
	(CollectionOf (Meet (TypedVariable (Variable "someone") (Type 'Concept))
		(Or
			(Present (State (Variable "someone") (Concept "thirsty")))
			(And
				(is-true (Edge (Predicate "cold") (Variable "someone"))))))))

(test-assert "thirsty or cold"
	(equal? (cog-execute! qr5) (Set (Concept "you") (Concept "me"))))

; ------------
(define qr6
	(CollectionOf (Meet (TypedVariable (Variable "someone") (Type 'Concept))
		(Or
			(Present (State (Variable "someone") (Concept "thirsty")))
			(is-true (Edge (Predicate "cold") (Variable "someone")))
			(is-true (Edge (Predicate "tired") (Variable "someone")))))))

(cog-set-value! (Edge (Predicate "tired") (Concept "her"))
	tvkey (BoolValue #f))

(test-assert "thirsty or cold but not tired"
	(equal? (cog-execute! qr6) (Set (Concept "you") (Concept "me"))))

; ------------
; Add the stv to force it to be strictly true.
(cog-set-value! (Edge (Predicate "tired") (Concept "her"))
	tvkey (BoolValue #t))

(test-assert "thirsty or cold or tired"
	(equal? (cog-execute! qr6)
		(Set (Concept "you") (Concept "me") (Concept "her"))))

; ------------
(cog-set-value! (Edge (Predicate "cold") (Concept "me"))
	tvkey (FloatValue 0.9 0.8))
(cog-set-value! (Edge (Predicate "tired") (Concept "her"))
	tvkey (FloatValue 0.6 0.1))

(define (strength-of ATOM) (ElementOf (Number 0) (ValueOf ATOM tvkey)))

(define qr7
	(CollectionOf (Meet (TypedVariable (Variable "someone") (Type 'Concept))
		(Or
			(Present (State (Variable "someone") (Concept "thirsty")))
			(GreaterThan (strength-of
					(Edge (Predicate "cold") (Variable "someone")))
				(Number 0.5))
			(GreaterThan (strength-of
					(Edge (Predicate "tired") (Variable "someone")))
				(Number 0.5))))))

(test-assert "strong tired no confidence"
	(equal? (cog-execute! qr7)
		(Set (Concept "you") (Concept "me") (Concept "her"))))

(define qr8
	(CollectionOf (Meet (TypedVariable (Variable "someone") (Type 'Concept))
		(Or
			(Present (State (Variable "someone") (Concept "thirsty")))
			(Not (GreaterThan (Number 0.5) (strength-of
				(Edge (Predicate "cold") (Variable "someone")))))
			(Not (GreaterThan (Number 0.5) (strength-of
				(Edge (Predicate "tired") (Variable "someone")))))))))

(test-assert "not strong tired no confidence"
	(equal? (cog-execute! qr8)
		(Set (Concept "you") (Concept "me") (Concept "her"))))

(test-end tname)

(opencog-test-end)
