(ImplicationScopeLink (stv 0.5 0.0025)
   (VariableList
      (VariableNode "$T")
      (TypedVariableLink
         (VariableNode "$A")
         (TypeNode "DontExecLink")
      )
      (VariableNode "$L")
      (TypedVariableLink
         (VariableNode "$B")
         (TypeNode "DontExecLink")
      )
   )
   (AndLink
      (ExecutionLink
         (SchemaNode "URE:BC:expand-and-BIT")
         (ListLink
            (VariableNode "$A")
            (VariableNode "$L")
            (DontExecLink
               (DefinedSchemaNode "rule-1")
            )
         )
         (VariableNode "$B")
      )
      (EvaluationLink
         (PredicateNode "URE:BC:preproof-of")
         (ListLink
            (VariableNode "$A")
            (VariableNode "$T")
         )
      )
   )
   (EvaluationLink
      (PredicateNode "URE:BC:preproof-of")
      (ListLink
         (VariableNode "$B")
         (VariableNode "$T")
      )
   )
)
