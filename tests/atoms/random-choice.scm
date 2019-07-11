;
; Unit test for several different things, that are not really
; tested anywhere else (or are poorly tested):
; -- RandomChoiceLink with weights
; -- DefinedPredicate and DefinedSchema
; -- DivideLink
; -- Tail recursion optimization in the SequentialAndLink
; -- Lazy/Eager execution of FunctionLink arguments.
;
(use-modules (opencog) (opencog exec))

; Pick A with 70% probability, pick B with 30% probability.
(Define (DefinedSchema "rand-transpose")
   (RandomChoice
      (List (Number 0.7) (Number 0.3))
      (List (Concept "A") (Concept "B"))))

; Pick A with 70% probability, pick B with 30% probability.
(Define (DefinedSchema "rand-set-choice")
   (RandomChoice
      (SetLink
         (List (Number 0.7) (Concept "A"))
         (List (Number 0.3) (Concept "B")))))

; Pick A with 70% probability, pick B with 30% probability.
; Use a Getlink to fish out the probabilities, instead of
; hard-coding them, as above. This forces the lazy/eager
; evaluation subsystem to "do the right thing" -- to evaluate
; the GetLink before running the RandomChoice.
(Define (DefinedSchema "randy")
   (RandomChoice
      (GetLink
         (VariableList (VariableNode "$prob") (VariableNode "$expr"))
         (AndLink
            (EvaluationLink
               (PredicateNode "Emotion-expression")
               (ListLink (ConceptNode "wake-up") (VariableNode "$expr")))
            (StateLink
               (ListLink
                  (ConceptNode "wake-up")
                  (VariableNode "$expr"))
               (VariableNode "$prob"))))
      ))

; Data to allow the GetLink above to find the needed data.
(Evaluation (Predicate "Emotion-expression")
   (ListLink (Concept "wake-up") (Concept "A")))
(State (ListLink (ConceptNode "wake-up") (Concept "A")) (Number 0.7))

(Evaluation (Predicate "Emotion-expression")
   (ListLink (Concept "wake-up") (Concept "B")))
(State (ListLink (ConceptNode "wake-up") (Concept "B")) (Number 0.3))

; Initialize counts to zero.
(State (Anchor "sum-A") (Number 0))
(State (Anchor "sum-B") (Number 0))

; Accumulate counts of how often A and B are picked.
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

; Expectation value is 0.7/0.3 = 2.33333
(Define (DefinedPredicate "test")
   (SequentialAnd
      (GreaterThan (Number 2.5) (DefinedSchema "ratio"))
      (GreaterThan (DefinedSchema "ratio") (Number 2.2))))

*unspecified*
