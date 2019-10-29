;
; random-choice.scm -- Using the RandomChoiceLink for sampling.
;
; This example uses the RandomChoiceLink to pick one of two alternatives,
; ConceptNode A or ConceptNode B, with a probability of 70-30.  It then
; counts how often each of these choices occurred.
;
(use-modules (opencog) (opencog exec))

; Pick A with 70% probability, pick B with 30% probability.
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

; Run this several times.
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

; Print the counts.
(cog-execute! (Get (State (Anchor "sum-A") (Variable "$x"))))
(cog-execute! (Get (State (Anchor "sum-B") (Variable "$x"))))

; Run it a thousand times.
(State (Anchor "loop-count") (Number 0))

; This defines a tail-recursive loop. The loop-counter is incremented,
; and, if it is less than a thousand, the function calls itself again.
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

; Actually execute the loop.
(cog-evaluate! (DefinedPredicate "loop a lot of times"))

; Print the counts again.
(cog-execute! (Get (State (Anchor "sum-A") (Variable "$x"))))
(cog-execute! (Get (State (Anchor "sum-B") (Variable "$x"))))

; Print the ratio.
(Define (DefinedSchema "ratio")
	(Divide
		(Get (State (Anchor "sum-A") (Variable "$x")))
		(Get (State (Anchor "sum-B") (Variable "$x")))))

(cog-execute! (DefinedSchema "ratio"))

; Test that the actual ratio is close to the expectation value.
; The expectation value is 0.7 / 0.3 = 2.33333 ...
; We should almost always, after a thousand times, get a ratio
; that is greater than 2.1 and less than 2.5.
(Define (DefinedPredicate "test expectation")
   (SequentialAnd
      (GreaterThan (Number 2.5) (DefinedSchema "ratio"))
      (GreaterThan (DefinedSchema "ratio") (NumberNode 2.1))))

(cog-evaluate! (DefinedPredicate "test expectation"))
