;
; Unit test for RandomChoiceLink
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

; Accumulate counts of howften A and B are picked.
(Define (DefinedPredicate "counter")
   (SequentialOr
      (SequentialAnd
         (Equal (DefinedSchema "randy") (Concept "A"))
         (True (Put
            (State (Anchor "sum-A") (Variable "$x"))
            (Plus (Number 1)
               (Get (State (Anchor "sum-A") (Variable "$y")))))))
      (True (Put
         (State (Anchor "sum-B") (Variable "$x"))
         (Plus (Number 1)
            (Get (State (Anchor "sum-B") (Variable "$y"))))))))

; Run it a few thousand times
(State (Anchor "loop-count") (Number 0))

(Define (DefinedPredicate "loop a lot of times")
   (SequentialAnd
      (DefinedPredicate "counter")
      (TrueLink (PutLink
         (State (Anchor "loop-count") (Variable "$x"))
         (Plus (Number 1) (Get (State (Anchor "loop-count") (Variable "$x"))))))
      (GreaterThan
         (Number 3000)
         (Get (State (Anchor "loop-count") (Variable "$x"))))
      (DefinedPredicate "loop a lot of times")))

; Print the ratio
(Define (DefinedSchema "ratio")
   (Divide
      (Get (State (Anchor "sum-A") (Variable "$x")))
      (Get (State (Anchor "sum-B") (Variable "$x")))))

; Expectation value os 0.7/0.3 = 2.33333
(Define (DefinedPredicate "test")
   (SequentialAnd
      (GreaterThan (Number 2.5) (DefinedSchema "ratio"))
      (GreaterThan (DefinedSchema "ratio") (Number 2.2))))
