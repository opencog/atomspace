
; SENTENCE: [The color of the sky is blue.]
; _predadj (<<color>>, <<blue>>)
(EvaluationLink
   (DefinedLinguisticRelationshipNode "_predadj")
   (ListLink
      (WordInstanceNode "color@8947b41d-b6d8-44c5-95a0-a909220596d5")
      (WordInstanceNode "blue@cf040834-cf7a-42ae-bd42-83a001d3c3e3")
   )
)
; of (<<color>>, <<sky>>)
(EvaluationLink
   (PrepositionalRelationshipNode "of")
   (ListLink
      (WordInstanceNode "color@8947b41d-b6d8-44c5-95a0-a909220596d5")
      (WordInstanceNode "sky@d30ab6dd-0785-4b14-8969-f23697d384a7")
   )
)


; The whole point here is that the AbsentLink means that no match at
; all should be found. i.e. $prep can match "of" and should thus be
; rejected.
;

(define (rule-good)
   (QueryLink
      (VariableList
         (TypedVariableLink
            (VariableNode "$var2")
            (TypeNode "WordInstanceNode")
         )
         (TypedVariableLink
            (VariableNode "$prep")
            (TypeNode "PrepositionalRelationshipNode")
         )
         (TypedVariableLink
             (VariableNode "$var3")
             (TypeNode "WordInstanceNode")
         )
         (VariableNode "$var1")
      )
      (AndLink
         (EvaluationLink
            (DefinedLinguisticRelationshipNode "_predadj")
            (ListLink
               (VariableNode "$var2")
               (VariableNode "$var1")
            )
         )
         (AbsentLink
            (EvaluationLink
               (VariableNode "$prep")
               (ListLink
                  (VariableNode "$var2")
                  (VariableNode "$var3")
               )
            )
         )
      )
      (ListLink
         (VariableNode "$var1")
         (VariableNode "$var2")
      )
   )
)


; This rule has an explicitly bad TypeNode --
; PreposxitionalRelationshipNode is misspelled (on purpose,
; since we want to test for the mis-spelled case).
(define (rule-bad)
   (QueryLink
      (VariableList
         (TypedVariableLink
            (VariableNode "$var2")
            (TypeNode "WordInstanceNode")
         )
         (TypedVariableLink
            (VariableNode "$prep")
            (TypeNode "PreposxitionalRelationshipNode")
         )
         (TypedVariableLink
             (VariableNode "$var3")
             (TypeNode "WordInstanceNode")
         )
         (VariableNode "$var1")
      )
      (AndLink
         (EvaluationLink
            (DefinedLinguisticRelationshipNode "_predadj")
            (ListLink
               (VariableNode "$var2")
               (VariableNode "$var1")
            )
         )
         (AbsentLink
            (EvaluationLink
               (VariableNode "$prep")
               (ListLink
                  (VariableNode "$var2")
                  (VariableNode "$var3")
               )
            )
         )
      )
      (ListLink
         (VariableNode "$var1")
         (VariableNode "$var2")
      )
   )
)
