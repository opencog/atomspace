;; For BITUtest

(define fcs-1
(BindLink
  (AndLink
    (NotLink
      (IdenticalLink
        (LambdaLink
          (TypedVariableLink
            (VariableNode "$X")
            (TypeNode "ConceptNode")
          )
          (EvaluationLink
            (PredicateNode "take")
            (ListLink
              (VariableNode "$X")
              (ConceptNode "treatment-1")
            )
          )
        )
        (LambdaLink
          (TypedVariableLink
            (VariableNode "$X")
            (TypeNode "ConceptNode")
          )
          (AndLink
            (EvaluationLink
              (PredicateNode "contain")
              (ListLink
                (ConceptNode "treatment-1")
                (ConceptNode "compound-A")
              )
            )
            (EvaluationLink
              (PredicateNode "take")
              (ListLink
                (VariableNode "$X")
                (ConceptNode "treatment-1")
              )
            )
          )
        )
      )
    )
    (NotLink
      (EqualLink
        (LambdaLink
          (TypedVariableLink
            (VariableNode "$X")
            (TypeNode "ConceptNode")
          )
          (EvaluationLink
            (PredicateNode "contain")
            (ListLink
              (ConceptNode "treatment-1")
              (ConceptNode "compound-A")
            )
          )
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
              (ConceptNode "treatment-1")
            )
          )
        )
      )
    )
    (EvaluationLink
      (GroundedPredicateNode "scm: implication-introduction-precondition")
      (ListLink
        (LambdaLink
          (TypedVariableLink
            (VariableNode "$X")
            (TypeNode "ConceptNode")
          )
          (EvaluationLink
            (PredicateNode "take")
            (ListLink
              (VariableNode "$X")
              (ConceptNode "treatment-1")
            )
          )
        )
        (LambdaLink
          (TypedVariableLink
            (VariableNode "$X")
            (TypeNode "ConceptNode")
          )
          (EvaluationLink
            (PredicateNode "contain")
            (ListLink
              (ConceptNode "treatment-1")
              (ConceptNode "compound-A")
            )
          )
        )
      )
    )
  )
  (ExecutionOutputLink
    (GroundedSchemaNode "scm: deduction-formula")
    (ListLink
      (ImplicationLink
        (LambdaLink
          (TypedVariableLink
            (VariableNode "$X")
            (TypeNode "ConceptNode")
          )
          (EvaluationLink
            (PredicateNode "take")
            (ListLink
              (VariableNode "$X")
              (ConceptNode "treatment-1")
            )
          )
        )
        (LambdaLink
          (TypedVariableLink
            (VariableNode "$X")
            (TypeNode "ConceptNode")
          )
          (AndLink
            (EvaluationLink
              (PredicateNode "contain")
              (ListLink
                (ConceptNode "treatment-1")
                (ConceptNode "compound-A")
              )
            )
            (EvaluationLink
              (PredicateNode "take")
              (ListLink
                (VariableNode "$X")
                (ConceptNode "treatment-1")
              )
            )
          )
        )
      )
      (ExecutionOutputLink
        (GroundedSchemaNode "scm: implication-implicant-distribution-formula")
        (ListLink
          (ImplicationLink
            (LambdaLink
              (TypedVariableLink
                (VariableNode "$X")
                (TypeNode "ConceptNode")
              )
              (EvaluationLink
                (PredicateNode "take")
                (ListLink
                  (VariableNode "$X")
                  (ConceptNode "treatment-1")
                )
              )
            )
            (AndLink
              (LambdaLink
                (TypedVariableLink
                  (VariableNode "$X")
                  (TypeNode "ConceptNode")
                )
                (EvaluationLink
                  (PredicateNode "contain")
                  (ListLink
                    (ConceptNode "treatment-1")
                    (ConceptNode "compound-A")
                  )
                )
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
                    (ConceptNode "treatment-1")
                  )
                )
              )
            )
          )
          (ExecutionOutputLink
            (GroundedSchemaNode "scm: implication-introduction-formula")
            (ListLink
              (ImplicationLink
                (LambdaLink
                  (TypedVariableLink
                    (VariableNode "$X")
                    (TypeNode "ConceptNode")
                  )
                  (EvaluationLink
                    (PredicateNode "take")
                    (ListLink
                      (VariableNode "$X")
                      (ConceptNode "treatment-1")
                    )
                  )
                )
                (LambdaLink
                  (TypedVariableLink
                    (VariableNode "$X")
                    (TypeNode "ConceptNode")
                  )
                  (EvaluationLink
                    (PredicateNode "contain")
                    (ListLink
                      (ConceptNode "treatment-1")
                      (ConceptNode "compound-A")
                    )
                  )
                )
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
                    (ConceptNode "treatment-1")
                  )
                )
              )
              (LambdaLink
                (TypedVariableLink
                  (VariableNode "$X")
                  (TypeNode "ConceptNode")
                )
                (EvaluationLink
                  (PredicateNode "contain")
                  (ListLink
                    (ConceptNode "treatment-1")
                    (ConceptNode "compound-A")
                  )
                )
              )
            )
          )
        )
      )
      (ExecutionOutputLink
        (GroundedSchemaNode "scm: implication-and-lambda-factorization-formula")
        (ImplicationLink
          (AndLink
            (LambdaLink
              (TypedVariableLink
                (VariableNode "$X")
                (TypeNode "ConceptNode")
              )
              (EvaluationLink
                (PredicateNode "contain")
                (ListLink
                  (ConceptNode "treatment-1")
                  (ConceptNode "compound-A")
                )
              )
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
                  (ConceptNode "treatment-1")
                )
              )
            )
          )
          (LambdaLink
            (TypedVariableLink
              (VariableNode "$X")
              (TypeNode "ConceptNode")
            )
            (AndLink
              (EvaluationLink
                (PredicateNode "contain")
                (ListLink
                  (ConceptNode "treatment-1")
                  (ConceptNode "compound-A")
                )
              )
              (EvaluationLink
                (PredicateNode "take")
                (ListLink
                  (VariableNode "$X")
                  (ConceptNode "treatment-1")
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

(define fcs-2
(BindLink
  (AndLink
    (NotLink
      (IdenticalLink
        (LambdaLink
          (TypedVariableLink
            (VariableNode "$X")
            (TypeNode "ConceptNode"))
          (EvaluationLink
            (PredicateNode "take")
            (ListLink
              (VariableNode "$X")
              (ConceptNode "treatment-1"))))
        (LambdaLink
          (TypedVariableLink
            (VariableNode "$X")
            (TypeNode "ConceptNode"))
          (AndLink
            (EvaluationLink
              (PredicateNode "contain")
              (ListLink
                (ConceptNode "treatment-1")
                (ConceptNode "compound-A")))
            (EvaluationLink
              (PredicateNode "take")
              (ListLink
                (VariableNode "$X")
                (ConceptNode "treatment-1")))))))
    (NotLink
      (EqualLink
        (LambdaLink
          (TypedVariableLink
            (VariableNode "$X")
            (TypeNode "ConceptNode"))
          (EvaluationLink
            (PredicateNode "contain")
            (ListLink
              (ConceptNode "treatment-1")
              (ConceptNode "compound-A"))))
        (LambdaLink
          (TypedVariableLink
            (VariableNode "$X")
            (TypeNode "ConceptNode"))
          (EvaluationLink
            (PredicateNode "take")
            (ListLink
              (VariableNode "$X")
              (ConceptNode "treatment-1"))))))
    (EvaluationLink
      (GroundedPredicateNode "scm: implication-introduction-precondition")
      (ListLink
        (LambdaLink
          (TypedVariableLink
            (VariableNode "$X")
            (TypeNode "ConceptNode"))
          (EvaluationLink
            (PredicateNode "take")
            (ListLink
              (VariableNode "$X")
              (ConceptNode "treatment-1"))))
        (LambdaLink
          (TypedVariableLink
            (VariableNode "$X")
            (TypeNode "ConceptNode")
          )
          (EvaluationLink
            (PredicateNode "contain")
            (ListLink
              (ConceptNode "treatment-1")
              (ConceptNode "compound-A")))))))
  (ExecutionOutputLink
    (GroundedSchemaNode "scm: deduction-formula")
    (ListLink
      (ImplicationLink
        (LambdaLink
          (TypedVariableLink
            (VariableNode "$X")
            (TypeNode "ConceptNode"))
          (EvaluationLink
            (PredicateNode "take")
            (ListLink
              (VariableNode "$X")
              (ConceptNode "treatment-1"))))
        (LambdaLink
          (TypedVariableLink
            (VariableNode "$X")
            (TypeNode "ConceptNode"))
          (AndLink
            (EvaluationLink
              (PredicateNode "contain")
              (ListLink
                (ConceptNode "treatment-1")
                (ConceptNode "compound-A")))
            (EvaluationLink
              (PredicateNode "take")
              (ListLink
                (VariableNode "$X")
                (ConceptNode "treatment-1"))))))
      (ExecutionOutputLink
        (GroundedSchemaNode "scm: implication-implicant-distribution-formula")
        (ListLink
          (ImplicationLink
            (LambdaLink
              (TypedVariableLink
                (VariableNode "$X")
                (TypeNode "ConceptNode"))
              (EvaluationLink
                (PredicateNode "take")
                (ListLink
                  (VariableNode "$X")
                  (ConceptNode "treatment-1"))))
            (AndLink
              (LambdaLink
                (TypedVariableLink
                  (VariableNode "$X")
                  (TypeNode "ConceptNode"))
                (EvaluationLink
                  (PredicateNode "contain")
                  (ListLink
                    (ConceptNode "treatment-1")
                    (ConceptNode "compound-A"))))
              (LambdaLink
                (TypedVariableLink
                  (VariableNode "$X")
                  (TypeNode "ConceptNode"))
                (EvaluationLink
                  (PredicateNode "take")
                  (ListLink
                    (VariableNode "$X")
                    (ConceptNode "treatment-1"))))))
          (ExecutionOutputLink
            (GroundedSchemaNode "scm: implication-introduction-formula")
            (ListLink
              (ImplicationLink
                (LambdaLink
                  (TypedVariableLink
                    (VariableNode "$X")
                    (TypeNode "ConceptNode"))
                  (EvaluationLink
                    (PredicateNode "take")
                    (ListLink
                      (VariableNode "$X")
                      (ConceptNode "treatment-1"))))
                (LambdaLink
                  (TypedVariableLink
                    (VariableNode "$X")
                    (TypeNode "ConceptNode"))
                  (EvaluationLink
                    (PredicateNode "contain")
                    (ListLink
                      (ConceptNode "treatment-1")
                      (ConceptNode "compound-A")))))
              (LambdaLink
                (TypedVariableLink
                  (VariableNode "$X")
                  (TypeNode "ConceptNode"))
                (EvaluationLink
                  (PredicateNode "take")
                  (ListLink
                    (VariableNode "$X")
                    (ConceptNode "treatment-1"))))
              (LambdaLink
                (TypedVariableLink
                  (VariableNode "$X")
                  (TypeNode "ConceptNode"))
                (EvaluationLink
                  (PredicateNode "contain")
                  (ListLink
                    (ConceptNode "treatment-1")
                    (ConceptNode "compound-A"))))))))
      (ExecutionOutputLink
        (GroundedSchemaNode "scm: implication-and-lambda-factorization-formula")
        (ImplicationLink
          (AndLink
            (LambdaLink
              (TypedVariableLink
                (VariableNode "$X")
                (TypeNode "ConceptNode"))
              (EvaluationLink
                (PredicateNode "contain")
                (ListLink
                  (ConceptNode "treatment-1")
                  (ConceptNode "compound-A"))))
            (LambdaLink
              (TypedVariableLink
                (VariableNode "$X")
                (TypeNode "ConceptNode"))
              (EvaluationLink
                (PredicateNode "take")
                (ListLink
                  (VariableNode "$X")
                  (ConceptNode "treatment-1")))))
          (LambdaLink
            (TypedVariableLink
              (VariableNode "$X")
              (TypeNode "ConceptNode"))
            (AndLink
              (EvaluationLink
                (PredicateNode "contain")
                (ListLink
                  (ConceptNode "treatment-1")
                  (ConceptNode "compound-A")))
              (EvaluationLink
                (PredicateNode "take")
                (ListLink
                  (VariableNode "$X")
                  (ConceptNode "treatment-1"))))))))))
)

(define fcs-3
(BindLink
  (VariableList
    (TypedVariableLink
      (VariableNode "$B-732fec2a")
      (TypeChoice
        (TypeNode "OrLink")
        (TypeNode "AndLink")
        (TypeNode "LambdaLink")
        (TypeNode "NotLink")
        (TypeNode "PredicateNode")
      )
    )
    (TypedVariableLink
      (VariableNode "$B-edaffcb")
      (TypeChoice
        (TypeNode "OrLink")
        (TypeNode "AndLink")
        (TypeNode "LambdaLink")
        (TypeNode "NotLink")
        (TypeNode "PredicateNode")
      )
    )
    (TypedVariableLink
      (VariableNode "$B-531e6c5d")
      (TypeChoice
        (TypeNode "OrLink")
        (TypeNode "AndLink")
        (TypeNode "LambdaLink")
        (TypeNode "NotLink")
        (TypeNode "PredicateNode")
      )
    )
  )
  (AndLink
    (ImplicationLink
      (VariableNode "$B-531e6c5d")
      (VariableNode "$B-edaffcb")
    )
    (ImplicationLink
      (LambdaLink
        (TypedVariableLink
          (VariableNode "$X")
          (TypeNode "ConceptNode")
        )
        (EvaluationLink
          (PredicateNode "take")
          (ListLink
            (VariableNode "$X")
            (ConceptNode "treatment-1")
          )
        )
      )
      (VariableNode "$B-531e6c5d")
    )
    (NotLink
      (IdenticalLink
        (LambdaLink
          (TypedVariableLink
            (VariableNode "$X")
            (TypeNode "ConceptNode")
          )
          (EvaluationLink
            (PredicateNode "take")
            (ListLink
              (VariableNode "$X")
              (ConceptNode "treatment-1")
            )
          )
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
    (NotLink
      (IdenticalLink
        (VariableNode "$B-732fec2a")
        (LambdaLink
          (TypedVariableLink
            (VariableNode "$X")
            (TypeNode "ConceptNode")
          )
          (EvaluationLink
            (PredicateNode "take")
            (ListLink
              (VariableNode "$X")
              (ConceptNode "treatment-1")
            )
          )
        )
      )
    )
    (ImplicationLink
      (VariableNode "$B-732fec2a")
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
        (VariableNode "$B-edaffcb")
        (LambdaLink
          (TypedVariableLink
            (VariableNode "$X")
            (TypeNode "ConceptNode")
          )
          (EvaluationLink
            (PredicateNode "take")
            (ListLink
              (VariableNode "$X")
              (ConceptNode "treatment-1")
            )
          )
        )
      )
    )
    (ImplicationLink
      (VariableNode "$B-edaffcb")
      (VariableNode "$B-732fec2a")
    )
  )
  (ExecutionOutputLink
    (GroundedSchemaNode "scm: deduction-formula")
    (ListLink
      (ImplicationLink
        (LambdaLink
          (TypedVariableLink
            (VariableNode "$X")
            (TypeNode "ConceptNode")
          )
          (EvaluationLink
            (PredicateNode "take")
            (ListLink
              (VariableNode "$X")
              (ConceptNode "treatment-1")
            )
          )
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
      (ExecutionOutputLink
        (GroundedSchemaNode "scm: deduction-formula")
        (ListLink
          (ImplicationLink
            (LambdaLink
              (TypedVariableLink
                (VariableNode "$X")
                (TypeNode "ConceptNode")
              )
              (EvaluationLink
                (PredicateNode "take")
                (ListLink
                  (VariableNode "$X")
                  (ConceptNode "treatment-1")
                )
              )
            )
            (VariableNode "$B-732fec2a")
          )
          (ExecutionOutputLink
            (GroundedSchemaNode "scm: deduction-formula")
            (ListLink
              (ImplicationLink
                (LambdaLink
                  (TypedVariableLink
                    (VariableNode "$X")
                    (TypeNode "ConceptNode")
                  )
                  (EvaluationLink
                    (PredicateNode "take")
                    (ListLink
                      (VariableNode "$X")
                      (ConceptNode "treatment-1")
                    )
                  )
                )
                (VariableNode "$B-edaffcb")
              )
              (ImplicationLink
                (LambdaLink
                  (TypedVariableLink
                    (VariableNode "$X")
                    (TypeNode "ConceptNode")
                  )
                  (EvaluationLink
                    (PredicateNode "take")
                    (ListLink
                      (VariableNode "$X")
                      (ConceptNode "treatment-1")
                    )
                  )
                )
                (VariableNode "$B-531e6c5d")
              )
              (ImplicationLink
                (VariableNode "$B-531e6c5d")
                (VariableNode "$B-edaffcb")
              )
            )
          )
          (ImplicationLink
            (VariableNode "$B-edaffcb")
            (VariableNode "$B-732fec2a")
          )
        )
      )
      (ImplicationLink
        (VariableNode "$B-732fec2a")
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

(define fcs-4
(BindLink
  (VariableList
    (TypedVariableLink
      (VariableNode "?ATTR1")
      (TypeChoice
        (TypeNode "ConceptNode")
        (TypeNode "SchemaNode")
        (TypeNode "PredicateNode")
      )
    )
    (TypedVariableLink
      (VariableNode "?ATTR1-734b1af5")
      (TypeChoice
        (TypeNode "ConceptNode")
        (TypeNode "SchemaNode")
        (TypeNode "PredicateNode")
      )
    )
    (TypedVariableLink
      (VariableNode "?ATTR2-23db64a4")
      (TypeChoice
        (TypeNode "ConceptNode")
        (TypeNode "SchemaNode")
        (TypeNode "PredicateNode")
      )
    )
    (TypedVariableLink
      (VariableNode "?X-522132e2")
      (TypeChoice
        (TypeNode "ConceptNode")
        (TypeNode "SchemaNode")
        (TypeNode "PredicateNode")
      )
    )
  )
  (AndLink
    (AndLink
      (EvaluationLink
        (PredicateNode "contraryAttribute")
        (ListLink
          (VariableNode "?ATTR1-734b1af5")
          (VariableNode "?ATTR2-23db64a4")
        )
      )
      (EvaluationLink
        (PredicateNode "property")
        (ListLink
          (VariableNode "?X-522132e2")
          (VariableNode "?ATTR1-734b1af5")
        )
      )
      (EvaluationLink
        (PredicateNode "property")
        (ListLink
          (VariableNode "?X-522132e2")
          (VariableNode "?ATTR2-23db64a4")
        )
      )
    )
    (EvaluationLink
      (PredicateNode "property")
      (ListLink
        (ConceptNode "TheKB2-1")
        (VariableNode "?ATTR1")
      )
    )
    (EvaluationLink
      (PredicateNode "contraryAttribute")
      (ListLink
        (VariableNode "?ATTR1")
        (ConceptNode "Inconsistent")
      )
    )
    (EvaluationLink
      (GroundedPredicateNode "scm: gt-zero-confidence")
      (AndLink
        (EvaluationLink
          (PredicateNode "contraryAttribute")
          (ListLink
            (VariableNode "?ATTR1-734b1af5")
            (VariableNode "?ATTR2-23db64a4")
          )
        )
        (EvaluationLink
          (PredicateNode "property")
          (ListLink
            (VariableNode "?X-522132e2")
            (VariableNode "?ATTR1-734b1af5")
          )
        )
        (EvaluationLink
          (PredicateNode "property")
          (ListLink
            (VariableNode "?X-522132e2")
            (VariableNode "?ATTR2-23db64a4")
          )
        )
      )
    )
  )
  (ExecutionOutputLink
    (GroundedSchemaNode "scm: conditional-full-instantiation-scope-formula")
    (ListLink
      (EvaluationLink
        (PredicateNode "property")
        (ListLink
          (ConceptNode "TheKB2-1")
          (ConceptNode "Inconsistent")
        )
      )
      (ExecutionOutputLink
        (GroundedSchemaNode "scm: fuzzy-conjunction-introduction-formula")
        (ListLink
          (AndLink
            (EvaluationLink
              (PredicateNode "property")
              (ListLink
                (ConceptNode "TheKB2-1")
                (ConceptNode "Inconsistent")
              )
            )
            (EvaluationLink
              (PredicateNode "property")
              (ListLink
                (ConceptNode "TheKB2-1")
                (VariableNode "?ATTR1")
              )
            )
            (EvaluationLink
              (PredicateNode "contraryAttribute")
              (ListLink
                (VariableNode "?ATTR1")
                (ConceptNode "Inconsistent")
              )
            )
          )
          (SetLink
            (EvaluationLink
              (PredicateNode "property")
              (ListLink
                (ConceptNode "TheKB2-1")
                (VariableNode "?ATTR1")
              )
            )
            (EvaluationLink
              (PredicateNode "contraryAttribute")
              (ListLink
                (VariableNode "?ATTR1")
                (ConceptNode "Inconsistent")
              )
            )
            (ExecutionOutputLink
              (GroundedSchemaNode "scm: conditional-full-instantiation-scope-formula")
              (ListLink
                (EvaluationLink
                  (PredicateNode "property")
                  (ListLink
                    (ConceptNode "TheKB2-1")
                    (ConceptNode "Inconsistent")
                  )
                )
                (AndLink
                  (EvaluationLink
                    (PredicateNode "contraryAttribute")
                    (ListLink
                      (VariableNode "?ATTR1-734b1af5")
                      (VariableNode "?ATTR2-23db64a4")
                    )
                  )
                  (EvaluationLink
                    (PredicateNode "property")
                    (ListLink
                      (VariableNode "?X-522132e2")
                      (VariableNode "?ATTR1-734b1af5")
                    )
                  )
                  (EvaluationLink
                    (PredicateNode "property")
                    (ListLink
                      (VariableNode "?X-522132e2")
                      (VariableNode "?ATTR2-23db64a4")
                    )
                  )
                )
                (ImplicationScopeLink
                  (VariableList
                    (TypedVariableLink
                      (VariableNode "?ATTR1")
                      (TypeChoice
                        (TypeNode "ConceptNode")
                        (TypeNode "SchemaNode")
                        (TypeNode "PredicateNode")
                      )
                    )
                    (TypedVariableLink
                      (VariableNode "?ATTR2")
                      (TypeChoice
                        (TypeNode "ConceptNode")
                        (TypeNode "SchemaNode")
                        (TypeNode "PredicateNode")
                      )
                    )
                    (TypedVariableLink
                      (VariableNode "?X")
                      (TypeChoice
                        (TypeNode "ConceptNode")
                        (TypeNode "SchemaNode")
                        (TypeNode "PredicateNode")
                      )
                    )
                  )
                  (AndLink
                    (EvaluationLink
                      (PredicateNode "property")
                      (ListLink
                        (VariableNode "?X")
                        (VariableNode "?ATTR1")
                      )
                    )
                    (EvaluationLink
                      (PredicateNode "property")
                      (ListLink
                        (VariableNode "?X")
                        (VariableNode "?ATTR2")
                      )
                    )
                    (EvaluationLink
                      (PredicateNode "contraryAttribute")
                      (ListLink
                        (VariableNode "?ATTR1")
                        (VariableNode "?ATTR2")
                      )
                    )
                  )
                  (EvaluationLink
                    (PredicateNode "property")
                    (ListLink
                      (ConceptNode "TheKB2-1")
                      (ConceptNode "Inconsistent")
                    )
                  )
                )
              )
            )
          )
        )
      )
      (ImplicationScopeLink
        (VariableList
          (TypedVariableLink
            (VariableNode "?ATTR1")
            (TypeChoice
              (TypeNode "ConceptNode")
              (TypeNode "SchemaNode")
              (TypeNode "PredicateNode")
            )
          )
          (TypedVariableLink
            (VariableNode "?ATTR2")
            (TypeChoice
              (TypeNode "ConceptNode")
              (TypeNode "SchemaNode")
              (TypeNode "PredicateNode")
            )
          )
          (TypedVariableLink
            (VariableNode "?X")
            (TypeChoice
              (TypeNode "ConceptNode")
              (TypeNode "SchemaNode")
              (TypeNode "PredicateNode")
            )
          )
        )
        (AndLink
          (EvaluationLink
            (PredicateNode "property")
            (ListLink
              (VariableNode "?X")
              (VariableNode "?ATTR1")
            )
          )
          (EvaluationLink
            (PredicateNode "property")
            (ListLink
              (VariableNode "?X")
              (VariableNode "?ATTR2")
            )
          )
          (EvaluationLink
            (PredicateNode "contraryAttribute")
            (ListLink
              (VariableNode "?ATTR1")
              (VariableNode "?ATTR2")
            )
          )
        )
        (EvaluationLink
          (PredicateNode "property")
          (ListLink
            (ConceptNode "TheKB2-1")
            (ConceptNode "Inconsistent")
          )
        )
      )
    )
  )
)
)

(define fcs-5
(BindLink
  (VariableList
    (TypedVariableLink
      (VariableNode "$g-2101505b")
      (TypeChoice
        (TypeNode "LambdaLink")
        (TypeNode "PutLink")
      )
    )
    (TypedVariableLink
      (VariableNode "$f-53364b8c")
      (TypeChoice
        (TypeNode "ConceptNode")
        (TypeNode "LambdaLink")
      )
    )
  )
  (AndLink
    (VariableNode "$f-53364b8c")
    (EvaluationLink
      (PredicateNode "minsup")
      (ListLink
        (VariableNode "$g-2101505b")
        (ConceptNode "texts")
        (NumberNode "2.000000")
      )
    )
    (EvaluationLink
      (GroundedPredicateNode "scm: has-arity")
      (ListLink
        (VariableNode "$g-2101505b")
        (NumberNode "2.000000")
      )
    )
    (EvaluationLink
      (GroundedPredicateNode "scm: absolutely-true")
      (EvaluationLink
        (PredicateNode "minsup")
        (ListLink
          (VariableNode "$g-2101505b")
          (ConceptNode "texts")
          (NumberNode "2.000000")
        )
      )
    )
  )
  (ExecutionOutputLink
    (GroundedSchemaNode "scm: specialization-formula")
    (ListLink
      (EvaluationLink
        (PredicateNode "minsup")
        (ListLink
          (QuoteLink
            (PutLink
              (UnquoteLink
                (VariableNode "$g-2101505b")
              )
              (UnquoteLink
                (ListLink
                  (VariableNode "$f-53364b8c")
                  (VariableNode "$spe-arg-1")
                )
              )
            )
          )
          (ConceptNode "texts")
          (NumberNode "2.000000")
        )
      )
      (EvaluationLink
        (PredicateNode "minsup")
        (ListLink
          (VariableNode "$g-2101505b")
          (ConceptNode "texts")
          (NumberNode "2.000000")
        )
      )
      (VariableNode "$f-53364b8c")
    )
  )
)
)
