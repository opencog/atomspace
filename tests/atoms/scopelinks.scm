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
      (GroundedPredicateNode "scm: implication-construction-precondition")
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
                    (GroundedSchemaNode "scm: implication-construction-formula")
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
      (GroundedPredicateNode "scm: implication-construction-precondition")
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
                (GroundedSchemaNode "scm: implication-construction-formula")
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
      (GroundedPredicateNode "scm: implication-construction-precondition")
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
      (GroundedPredicateNode "scm: implication-construction-precondition")
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

