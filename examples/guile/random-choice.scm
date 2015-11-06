;
; random-choice.scm -- Example use of the RandomChoiceLink
;
; This example uses the RadomChoiceLink to pick one of two alteratives,
; ConceptNode A or ConceptNode B, with a probability of 70-30.  It then
; couns how often each of these choices occured.
;
(use-modules (opencog) (opencog exec))

; Pick A with 70% probability, pick B with 30% probability
(Define (DefinedSchema "randy")
	(RandomChoice
		(List (Number 0.7) (Number 0.3))
		(List (Concept "A") (Concept "B"))))

; Initialize counts to zero.
(State (Anchor "sum-A") (Number 0))
(State (Anchor "sum-B") (Number 0))

; Accumulate counts of how often A and B are picked.
(Define (DefinedPredicate "counter")
	(SequentialOr
		(SequentialAnd
			; If A was picked...
			(Equal (DefinedSchema "randy") (Concept "A"))
			; ... then increment the count of A ...
			(True (Put
				(State (Anchor "sum-A") (Variable "$x"))
				(Plus (Number 1)
					(Get (State (Anchor "sum-A") (Variable "$y")))))))
		; ... else increment the count of B.
		(True (Put
			(State (Anchor "sum-B") (Variable "$x"))
			(Plus (Number 1)
				(Get (State (Anchor "sum-B") (Variable "$y"))))))))

; Run this several times
(cog-evaluate! (DefinedPredicate "counter"))
(cog-evaluate! (DefinedPredicate "counter"))
(cog-evaluate! (DefinedPredicate "counter"))
(cog-evaluate! (DefinedPredicate "counter"))
(cog-evaluate! (DefinedPredicate "counter"))
(cog-evaluate! (DefinedPredicate "counter"))
(cog-evaluate! (DefinedPredicate "counter"))
(cog-evaluate! (DefinedPredicate "counter"))
(cog-evaluate! (DefinedPredicate "counter"))
(cog-evaluate! (DefinedPredicate "counter"))

; Print the counts
(cog-execute! (Get (State (Anchor "sum-A") (Variable "$x"))))
(cog-execute! (Get (State (Anchor "sum-B") (Variable "$x"))))

; Run it a thousand times
(State (Anchor "loop-count") (Number 0))

(Define (DefinedPredicate "loop a lot of times")
	(SequentialAnd
		(DefinedPredicate "counter")
		(TrueLink (PutLink
			(State (Anchor "loop-count") (Variable "$x"))
			(Plus (Number 1) (Get (State (Anchor "loop-count") (Variable "$x"))))))
		(GreaterThan
			(Number 1000)
			(Get (State (Anchor "loop-count") (Variable "$x"))))
		(DefinedPredicate "loop a lot of times")))

(cog-evaluate! (DefinedPredicate "loop a lot of times"))

; Print the counts again
(cog-execute! (Get (State (Anchor "sum-A") (Variable "$x"))))
(cog-execute! (Get (State (Anchor "sum-B") (Variable "$x"))))

; Print the ratio
(Define (DefinedSchema "ratio")
	(Divide
		(Get (State (Anchor "sum-A") (Variable "$x")))
		(Get (State (Anchor "sum-B") (Variable "$x")))))

(cog-execute! (DefinedSchema "ratio"))

; Test that the expectation value is bounded.
(Define (DefinedPredicate "test expectation")
   (SequentialAnd
      (GreaterThan (Number 2.5) (DefinedSchema "ratio"))
      (GreaterThan (DefinedSchema "ratio") (NumberNode 2.1))))

(cog-evaluate! (DefinedPredicate "test expectation"))
