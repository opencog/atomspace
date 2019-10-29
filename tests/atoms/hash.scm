;; Atoms for testing hash collisions

;; Scope links

;; These 2 bind links are not alpha-equivalent and should ideally have
;; different hash values.

(define bl-1
(BindLink
  (VariableList
    (TypedVariableLink
      (VariableNode "$B-a083600")
      (TypeChoice
        (TypeNode "OrLink")
        (TypeNode "AndLink")
        (TypeNode "LambdaLink")
        (TypeNode "NotLink")
        (TypeNode "PredicateNode")
      )
    )
    (TypedVariableLink
      (VariableNode "$B-5560bed3")
      (TypeChoice
        (TypeNode "OrLink")
        (TypeNode "AndLink")
        (TypeNode "LambdaLink")
        (TypeNode "NotLink")
        (TypeNode "PredicateNode")
      )
    )
    (TypedVariableLink
      (VariableNode "$Q-5fa8b92b")
      (TypeChoice
        (TypeNode "LambdaLink")
        (TypeNode "PredicateNode")
      )
    )
  )
  (AndLink
    (VariableNode "$B-a083600")
    (VariableNode "$Q-5fa8b92b")
    (NotLink
      (EqualLink
        (VariableNode "$B-a083600")
        (VariableNode "$Q-5fa8b92b")
      )
    )
    (NotLink
      (IdenticalLink
        (VariableNode "$B-a083600")
        (LambdaLink
          (TypedVariableLink
            (VariableNode "$X")
            (TypeNode "ConceptNode")
          )
          (EvaluationLink
            (PredicateNode "take")
            (ListLink
              (VariableNode "$X")
              (ConceptNode "compound-A")
            )
          )
        )
      )
    )
    (EvaluationLink
      (GroundedPredicateNode "scm: gt-zero-confidence")
      (EquivalenceLink
        (PredicateNode "take-treatment-1")
        (VariableNode "$B-a083600")
      )
    )
    (ImplicationLink
      (AndLink
        (VariableNode "$B-a083600")
        (VariableNode "$Q-5fa8b92b")
      )
      (VariableNode "$B-5560bed3")
    )
    (ImplicationLink
      (VariableNode "$B-5560bed3")
      (LambdaLink
        (TypedVariableLink
          (VariableNode "$X")
          (TypeNode "ConceptNode")
        )
        (EvaluationLink
          (PredicateNode "take")
          (ListLink
            (VariableNode "$X")
            (ConceptNode "compound-A")
          )
        )
      )
    )
    (EvaluationLink
      (GroundedPredicateNode "scm: implication-introduction-precondition")
      (ListLink
        (VariableNode "$B-a083600")
        (VariableNode "$Q-5fa8b92b")
      )
    )
    (NotLink
      (IdenticalLink
        (PredicateNode "take-treatment-1")
        (LambdaLink
          (TypedVariableLink
            (VariableNode "$X")
            (TypeNode "ConceptNode")
          )
          (EvaluationLink
            (PredicateNode "take")
            (ListLink
              (VariableNode "$X")
              (ConceptNode "compound-A")
            )
          )
        )
      )
    )
    (NotLink
      (IdenticalLink
        (VariableNode "$B-5560bed3")
        (VariableNode "$B-a083600")
      )
    )
    (EquivalenceLink
      (PredicateNode "take-treatment-1")
      (VariableNode "$B-a083600")
    )
  )
  (ExecutionOutputLink
    (GroundedSchemaNode "scm: deduction-formula")
    (ListLink
      (ImplicationLink
        (PredicateNode "take-treatment-1")
        (LambdaLink
          (TypedVariableLink
            (VariableNode "$X")
            (TypeNode "ConceptNode")
          )
          (EvaluationLink
            (PredicateNode "take")
            (ListLink
              (VariableNode "$X")
              (ConceptNode "compound-A")
            )
          )
        )
      )
      (ExecutionOutputLink
        (GroundedSchemaNode "scm: equivalence-to-implication-formula")
        (ListLink
          (ImplicationLink
            (PredicateNode "take-treatment-1")
            (VariableNode "$B-a083600")
          )
          (EquivalenceLink
            (PredicateNode "take-treatment-1")
            (VariableNode "$B-a083600")
          )
        )
      )
      (ExecutionOutputLink
        (GroundedSchemaNode "scm: deduction-formula")
        (ListLink
          (ImplicationLink
            (VariableNode "$B-a083600")
            (LambdaLink
              (TypedVariableLink
                (VariableNode "$X")
                (TypeNode "ConceptNode")
              )
              (EvaluationLink
                (PredicateNode "take")
                (ListLink
                  (VariableNode "$X")
                  (ConceptNode "compound-A")
                )
              )
            )
          )
          (ExecutionOutputLink
            (GroundedSchemaNode "scm: deduction-formula")
            (ListLink
              (ImplicationLink
                (VariableNode "$B-a083600")
                (VariableNode "$B-5560bed3")
              )
              (ExecutionOutputLink
                (GroundedSchemaNode "scm: implication-implicant-distribution-formula")
                (ListLink
                  (ImplicationLink
                    (VariableNode "$B-a083600")
                    (AndLink
                      (VariableNode "$B-a083600")
                      (VariableNode "$Q-5fa8b92b")
                    )
                  )
                  (ExecutionOutputLink
                    (GroundedSchemaNode "scm: implication-introduction-formula")
                    (ListLink
                      (ImplicationLink
                        (VariableNode "$B-a083600")
                        (VariableNode "$Q-5fa8b92b")
                      )
                      (VariableNode "$B-a083600")
                      (VariableNode "$Q-5fa8b92b")
                    )
                  )
                )
              )
              (ImplicationLink
                (AndLink
                  (VariableNode "$B-a083600")
                  (VariableNode "$Q-5fa8b92b")
                )
                (VariableNode "$B-5560bed3")
              )
            )
          )
          (ImplicationLink
            (VariableNode "$B-5560bed3")
            (LambdaLink
              (TypedVariableLink
                (VariableNode "$X")
                (TypeNode "ConceptNode")
              )
              (EvaluationLink
                (PredicateNode "take")
                (ListLink
                  (VariableNode "$X")
                  (ConceptNode "compound-A")
                )
              )
            )
          )
        )
      )
    )
  )
)
)

(define bl-2
(BindLink
  (VariableList
    (TypedVariableLink
      (VariableNode "$B-a083600")
      (TypeChoice
        (TypeNode "OrLink")
        (TypeNode "AndLink")
        (TypeNode "LambdaLink")
        (TypeNode "NotLink")
        (TypeNode "PredicateNode")
      )
    )
    (TypedVariableLink
      (VariableNode "$B-79625a3")
      (TypeChoice
        (TypeNode "OrLink")
        (TypeNode "AndLink")
        (TypeNode "LambdaLink")
        (TypeNode "NotLink")
        (TypeNode "PredicateNode")
      )
    )
    (TypedVariableLink
      (VariableNode "$Q-1d91711a")
      (TypeChoice
        (TypeNode "LambdaLink")
        (TypeNode "PredicateNode")
      )
    )
  )
  (AndLink
    (VariableNode "$B-a083600")
    (VariableNode "$Q-1d91711a")
    (NotLink
      (EqualLink
        (VariableNode "$B-a083600")
        (VariableNode "$Q-1d91711a")
      )
    )
    (NotLink
      (IdenticalLink
        (VariableNode "$B-a083600")
        (LambdaLink
          (TypedVariableLink
            (VariableNode "$X")
            (TypeNode "ConceptNode")
          )
          (EvaluationLink
            (PredicateNode "take")
            (ListLink
              (VariableNode "$X")
              (ConceptNode "compound-A")
            )
          )
        )
      )
    )
    (ImplicationLink
      (PredicateNode "take-treatment-1")
      (VariableNode "$B-79625a3")
    )
    (EvaluationLink
      (GroundedPredicateNode "scm: gt-zero-confidence")
      (EquivalenceLink
        (VariableNode "$B-a083600")
        (VariableNode "$B-79625a3")
      )
    )
    (EvaluationLink
      (GroundedPredicateNode "scm: implication-introduction-precondition")
      (ListLink
        (VariableNode "$B-a083600")
        (VariableNode "$Q-1d91711a")
      )
    )
    (NotLink
      (IdenticalLink
        (PredicateNode "take-treatment-1")
        (LambdaLink
          (TypedVariableLink
            (VariableNode "$X")
            (TypeNode "ConceptNode")
          )
          (EvaluationLink
            (PredicateNode "take")
            (ListLink
              (VariableNode "$X")
              (ConceptNode "compound-A")
            )
          )
        )
      )
    )
    (EquivalenceLink
      (VariableNode "$B-a083600")
      (VariableNode "$B-79625a3")
    )
    (ImplicationLink
      (AndLink
        (VariableNode "$B-a083600")
        (VariableNode "$Q-1d91711a")
      )
      (LambdaLink
        (TypedVariableLink
          (VariableNode "$X")
          (TypeNode "ConceptNode")
        )
        (EvaluationLink
          (PredicateNode "take")
          (ListLink
            (VariableNode "$X")
            (ConceptNode "compound-A")
          )
        )
      )
    )
    (NotLink
      (IdenticalLink
        (PredicateNode "take-treatment-1")
        (VariableNode "$B-a083600")
      )
    )
  )
  (ExecutionOutputLink
    (GroundedSchemaNode "scm: deduction-formula")
    (ListLink
      (ImplicationLink
        (PredicateNode "take-treatment-1")
        (LambdaLink
          (TypedVariableLink
            (VariableNode "$X")
            (TypeNode "ConceptNode")
          )
          (EvaluationLink
            (PredicateNode "take")
            (ListLink
              (VariableNode "$X")
              (ConceptNode "compound-A")
            )
          )
        )
      )
      (ExecutionOutputLink
        (GroundedSchemaNode "scm: deduction-formula")
        (ListLink
          (ImplicationLink
            (PredicateNode "take-treatment-1")
            (VariableNode "$B-a083600")
          )
          (ImplicationLink
            (PredicateNode "take-treatment-1")
            (VariableNode "$B-79625a3")
          )
          (ExecutionOutputLink
            (GroundedSchemaNode "scm: equivalence-to-implication-formula")
            (ListLink
              (ImplicationLink
                (VariableNode "$B-79625a3")
                (VariableNode "$B-a083600")
              )
              (EquivalenceLink
                (VariableNode "$B-a083600")
                (VariableNode "$B-79625a3")
              )
            )
          )
        )
      )
      (ExecutionOutputLink
        (GroundedSchemaNode "scm: deduction-formula")
        (ListLink
          (ImplicationLink
            (VariableNode "$B-a083600")
            (LambdaLink
              (TypedVariableLink
                (VariableNode "$X")
                (TypeNode "ConceptNode")
              )
              (EvaluationLink
                (PredicateNode "take")
                (ListLink
                  (VariableNode "$X")
                  (ConceptNode "compound-A")
                )
              )
            )
          )
          (ExecutionOutputLink
            (GroundedSchemaNode "scm: implication-implicant-distribution-formula")
            (ListLink
              (ImplicationLink
                (VariableNode "$B-a083600")
                (AndLink
                  (VariableNode "$B-a083600")
                  (VariableNode "$Q-1d91711a")
                )
              )
              (ExecutionOutputLink
                (GroundedSchemaNode "scm: implication-introduction-formula")
                (ListLink
                  (ImplicationLink
                    (VariableNode "$B-a083600")
                    (VariableNode "$Q-1d91711a")
                  )
                  (VariableNode "$B-a083600")
                  (VariableNode "$Q-1d91711a")
                )
              )
            )
          )
          (ImplicationLink
            (AndLink
              (VariableNode "$B-a083600")
              (VariableNode "$Q-1d91711a")
            )
            (LambdaLink
              (TypedVariableLink
                (VariableNode "$X")
                (TypeNode "ConceptNode")
              )
              (EvaluationLink
                (PredicateNode "take")
                (ListLink
                  (VariableNode "$X")
                  (ConceptNode "compound-A")
                )
              )
            )
          )
        )
      )
    )
  )
)
)

(define bl-3
(BindLink
  (TypedVariableLink
    (GlobNode "$star")
    (IntervalLink
      (NumberNode "0.000000")
      (NumberNode "1.000000")
    )
  )
  (ListLink
    (ConceptNode "I")
    (ConceptNode "love")
    (GlobNode "$star")
  )
  (ListLink
    (ConceptNode "Hey!")
    (ConceptNode "I")
    (ConceptNode "like")
    (GlobNode "$star")
    (ConceptNode "also")
  )
)
)

(define bl-4
(BindLink
  (ListLink
    (ConceptNode "I")
    (ConceptNode "love")
    (GlobNode "$star")
  )
  (ListLink
    (ConceptNode "Hey!")
    (ConceptNode "I")
    (ConceptNode "like")
    (GlobNode "$star")
    (ConceptNode "also")
  )
)
)

(define bl-5
(BindLink
  (TypedVariableLink
    (VariableNode "$x")
    (TypeNode "ConceptNode")
  )
  (ListLink
    (ConceptNode "I")
    (ConceptNode "love")
    (VariableNode "$x")
  )
  (ListLink
    (ConceptNode "Hey!")
    (ConceptNode "I")
    (ConceptNode "like")
    (VariableNode "$x")
    (ConceptNode "also")
  )
)
)

(define bl-6
(BindLink
  (ListLink
    (ConceptNode "I")
    (ConceptNode "love")
    (VariableNode "$x")
  )
  (ListLink
    (ConceptNode "Hey!")
    (ConceptNode "I")
    (ConceptNode "like")
    (VariableNode "$x")
    (ConceptNode "also")
  )
)
)

;; These 2 get links are not alpha-equivalent and should ideally have
;; different hash values.

(define gl-1
(GetLink
  (VariableList
    (TypedVariableLink
      (VariableNode "$B-a083600")
      (TypeChoice
        (TypeNode "OrLink")
        (TypeNode "AndLink")
        (TypeNode "LambdaLink")
        (TypeNode "NotLink")
        (TypeNode "PredicateNode")
      )
    )
    (TypedVariableLink
      (VariableNode "$B-5560bed3")
      (TypeChoice
        (TypeNode "OrLink")
        (TypeNode "AndLink")
        (TypeNode "LambdaLink")
        (TypeNode "NotLink")
        (TypeNode "PredicateNode")
      )
    )
    (TypedVariableLink
      (VariableNode "$Q-5fa8b92b")
      (TypeChoice
        (TypeNode "LambdaLink")
        (TypeNode "PredicateNode")
      )
    )
  )
  (AndLink
    (VariableNode "$B-a083600")
    (VariableNode "$Q-5fa8b92b")
    (NotLink
      (EqualLink
        (VariableNode "$B-a083600")
        (VariableNode "$Q-5fa8b92b")
      )
    )
    (NotLink
      (IdenticalLink
        (VariableNode "$B-a083600")
        (LambdaLink
          (TypedVariableLink
            (VariableNode "$X")
            (TypeNode "ConceptNode")
          )
          (EvaluationLink
            (PredicateNode "take")
            (ListLink
              (VariableNode "$X")
              (ConceptNode "compound-A")
            )
          )
        )
      )
    )
    (EvaluationLink
      (GroundedPredicateNode "scm: gt-zero-confidence")
      (EquivalenceLink
        (PredicateNode "take-treatment-1")
        (VariableNode "$B-a083600")
      )
    )
    (ImplicationLink
      (AndLink
        (VariableNode "$B-a083600")
        (VariableNode "$Q-5fa8b92b")
      )
      (VariableNode "$B-5560bed3")
    )
    (ImplicationLink
      (VariableNode "$B-5560bed3")
      (LambdaLink
        (TypedVariableLink
          (VariableNode "$X")
          (TypeNode "ConceptNode")
        )
        (EvaluationLink
          (PredicateNode "take")
          (ListLink
            (VariableNode "$X")
            (ConceptNode "compound-A")
          )
        )
      )
    )
    (EvaluationLink
      (GroundedPredicateNode "scm: implication-introduction-precondition")
      (ListLink
        (VariableNode "$B-a083600")
        (VariableNode "$Q-5fa8b92b")
      )
    )
    (NotLink
      (IdenticalLink
        (PredicateNode "take-treatment-1")
        (LambdaLink
          (TypedVariableLink
            (VariableNode "$X")
            (TypeNode "ConceptNode")
          )
          (EvaluationLink
            (PredicateNode "take")
            (ListLink
              (VariableNode "$X")
              (ConceptNode "compound-A")
            )
          )
        )
      )
    )
    (NotLink
      (IdenticalLink
        (VariableNode "$B-5560bed3")
        (VariableNode "$B-a083600")
      )
    )
    (EquivalenceLink
      (PredicateNode "take-treatment-1")
      (VariableNode "$B-a083600")
    )
  )
)
)

(define gl-2
(GetLink
  (VariableList
    (TypedVariableLink
      (VariableNode "$B-a083600")
      (TypeChoice
        (TypeNode "OrLink")
        (TypeNode "AndLink")
        (TypeNode "LambdaLink")
        (TypeNode "NotLink")
        (TypeNode "PredicateNode")
      )
    )
    (TypedVariableLink
      (VariableNode "$B-79625a3")
      (TypeChoice
        (TypeNode "OrLink")
        (TypeNode "AndLink")
        (TypeNode "LambdaLink")
        (TypeNode "NotLink")
        (TypeNode "PredicateNode")
      )
    )
    (TypedVariableLink
      (VariableNode "$Q-1d91711a")
      (TypeChoice
        (TypeNode "LambdaLink")
        (TypeNode "PredicateNode")
      )
    )
  )
  (AndLink
    (VariableNode "$B-a083600")
    (VariableNode "$Q-1d91711a")
    (NotLink
      (EqualLink
        (VariableNode "$B-a083600")
        (VariableNode "$Q-1d91711a")
      )
    )
    (NotLink
      (IdenticalLink
        (VariableNode "$B-a083600")
        (LambdaLink
          (TypedVariableLink
            (VariableNode "$X")
            (TypeNode "ConceptNode")
          )
          (EvaluationLink
            (PredicateNode "take")
            (ListLink
              (VariableNode "$X")
              (ConceptNode "compound-A")
            )
          )
        )
      )
    )
    (ImplicationLink
      (PredicateNode "take-treatment-1")
      (VariableNode "$B-79625a3")
    )
    (EvaluationLink
      (GroundedPredicateNode "scm: gt-zero-confidence")
      (EquivalenceLink
        (VariableNode "$B-a083600")
        (VariableNode "$B-79625a3")
      )
    )
    (EvaluationLink
      (GroundedPredicateNode "scm: implication-introduction-precondition")
      (ListLink
        (VariableNode "$B-a083600")
        (VariableNode "$Q-1d91711a")
      )
    )
    (NotLink
      (IdenticalLink
        (PredicateNode "take-treatment-1")
        (LambdaLink
          (TypedVariableLink
            (VariableNode "$X")
            (TypeNode "ConceptNode")
          )
          (EvaluationLink
            (PredicateNode "take")
            (ListLink
              (VariableNode "$X")
              (ConceptNode "compound-A")
            )
          )
        )
      )
    )
    (EquivalenceLink
      (VariableNode "$B-a083600")
      (VariableNode "$B-79625a3")
    )
    (ImplicationLink
      (AndLink
        (VariableNode "$B-a083600")
        (VariableNode "$Q-1d91711a")
      )
      (LambdaLink
        (TypedVariableLink
          (VariableNode "$X")
          (TypeNode "ConceptNode")
        )
        (EvaluationLink
          (PredicateNode "take")
          (ListLink
            (VariableNode "$X")
            (ConceptNode "compound-A")
          )
        )
      )
    )
    (NotLink
      (IdenticalLink
        (PredicateNode "take-treatment-1")
        (VariableNode "$B-a083600")
      )
    )
  )
)
)

(define ll-1
(LambdaLink
    (VariableList
        (VariableNode "$PM-565f9848")
        (VariableNode "$PM-164e1b09")
    )
    (ImplicationLink
        (InheritanceLink
            (VariableNode "$PM-565f9848")
            (VariableNode "$PM-164e1b09")
        )
        (InheritanceLink
            (VariableNode "$PM-565f9848")
            (VariableNode "$PM-164e1b09")
        )
    )
)
)

(define ll-2
(LambdaLink
    (VariableList
        (VariableNode "$PM-164e1b09")
        (VariableNode "$PM-164e1b09-47189056")
    )
    (ImplicationLink
        (InheritanceLink
            (VariableNode "$PM-164e1b09")
            (VariableNode "$PM-164e1b09")
        )
        (InheritanceLink
            (VariableNode "$PM-164e1b09-47189056")
            (VariableNode "$PM-164e1b09-47189056")
        )
    )
)
)

(define ll-3
(LambdaLink
    (VariableList
        (VariableNode "$PM-41da0db7-3475e0e1")
        (VariableNode "$PM-61bb02a1")
        (VariableNode "$PM-61bb02a1-5779870f")
    )
    (ImplicationLink
        (InheritanceLink
            (VariableNode "$PM-41da0db7-3475e0e1")
            (VariableNode "$PM-61bb02a1")
        )
        (InheritanceLink
            (VariableNode "$PM-41da0db7-3475e0e1")
            (VariableNode "$PM-61bb02a1-5779870f")
        )
    )
)
)

(define ll-4
(LambdaLink
    (VariableList
        (VariableNode "$PM-61bb02a1-5779870f")
        (VariableNode "$PM-61bb02a1")
        (VariableNode "$PM-41da0db7-3475e0e1")
    )
    (ImplicationLink
        (InheritanceLink
            (VariableNode "$PM-61bb02a1-5779870f")
            (VariableNode "$PM-61bb02a1")
        )
        (InheritanceLink
            (VariableNode "$PM-41da0db7-3475e0e1")
            (VariableNode "$PM-61bb02a1-5779870f")
        )
    )
)
)

(define ll-5
(LambdaLink
    (VariableList
        (VariableNode "$PM-61bb02a1-5779870f")
        (VariableNode "$PM-61bb02a1")
    )
    (ImplicationLink
        (InheritanceLink
            (VariableNode "$PM-61bb02a1-5779870f")
            (VariableNode "$PM-61bb02a1")
        )
        (InheritanceLink
            (VariableNode "$PM-61bb02a1-5779870f")
            (VariableNode "$PM-61bb02a1-5779870f")
        )
    )
)
)

(define ll-6
(LambdaLink
    (VariableList
        (VariableNode "$PM-61bb02a1")
        (VariableNode "$PM-61bb02a1-5779870f")
    )
    (ImplicationLink
        (InheritanceLink
            (VariableNode "$PM-61bb02a1")
            (VariableNode "$PM-61bb02a1")
        )
        (InheritanceLink
            (VariableNode "$PM-61bb02a1")
            (VariableNode "$PM-61bb02a1-5779870f")
        )
    )
)
)

;; Non scope links

(define sal
(SequentialAndLink
  (AbsentLink
    (EvaluationLink
      (PredicateNode "visible")
      (ListLink
        (VariableNode "$x")
      )
    )
  )
  (EvaluationLink
    (GroundedPredicateNode "scm: incr-trig")
    (ListLink
    )
  )
)
)

(define sol
(SequentialOrLink
  (PresentLink
    (EvaluationLink
      (PredicateNode "visible")
      (ListLink
        (VariableNode "$x")
      )
    )
  )
  (EvaluationLink
    (GroundedPredicateNode "scm: incr-trig")
    (ListLink
    )
  )
)
)

(define tl
(TimesLink
  (NumberNode "5.000000")
  (PlusLink
    (NumberNode "3.000000")
    (ValueOfLink
      (ConceptNode "some atom")
      (PredicateNode "my key")
    )
  )
)
)

(define pl
(PlusLink
  (NumberNode "5.000000")
  (TimesLink
    (NumberNode "3.000000")
    (ValueOfLink
      (ConceptNode "some atom")
      (PredicateNode "my key")
    )
  )
)
)
