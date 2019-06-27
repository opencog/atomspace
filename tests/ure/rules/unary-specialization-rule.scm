(define unary-specialization-rule
(BindLink
  (VariableList
    (TypedVariableLink
      (VariableNode "$g")
      (TypeChoice
        (TypeNode "LambdaLink")
        (TypeNode "PutLink")))
    (TypedVariableLink
      (VariableNode "$texts")
      (TypeNode "ConceptNode"))
    (TypedVariableLink
      (VariableNode "$ms")
      (TypeNode "NumberNode"))
    (TypedVariableLink
      (VariableNode "$f")
      (TypeChoice
        (TypeNode "LambdaLink")
        (TypeNode "ConceptNode"))))
  (AndLink
    (VariableNode "$f")
    (EvaluationLink
      (GroundedPredicateNode "scm: has-arity")
      (ListLink
        (VariableNode "$g")
        (NumberNode "1.000000")))
    (EvaluationLink
      (PredicateNode "minsup")
      (ListLink
        (VariableNode "$g")
        (VariableNode "$texts")
        (VariableNode "$ms")))
    (EvaluationLink
      (GroundedPredicateNode "scm: absolutely-true")
      (EvaluationLink
        (PredicateNode "minsup")
        (ListLink
          (VariableNode "$g")
          (VariableNode "$texts")
          (VariableNode "$ms")))))
  (ExecutionOutputLink
    (GroundedSchemaNode "scm: specialization-formula")
    (ListLink
      (EvaluationLink
        (PredicateNode "minsup")
        (ListLink
          (QuoteLink
            (PutLink
              (UnquoteLink
                (VariableNode "$g"))
              (UnquoteLink
                (VariableNode "$f"))))
          (VariableNode "$texts")
          (VariableNode "$ms")))
      (EvaluationLink
        (PredicateNode "minsup")
        (ListLink
          (VariableNode "$g")
          (VariableNode "$texts")
          (VariableNode "$ms")))
      (VariableNode "$f")))))

(define unary-specialization-rule-name (DefinedSchemaNode "unary-specialization-rule"))
(DefineLink unary-specialization-rule-name unary-specialization-rule)
