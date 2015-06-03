(ImplicationLink (stv 1.0 1.0)
 (AndLink
  (EvaluationLink 
   (PredicateNode "croaks")
   (VariableNode "$X")
  )
  (EvaluationLink     
    (PredicateNode "eats_flies")
    (VariableNode "$X")
  )
 )
 (InheritanceLink
  (VariableNode "$X")
  (ConceptNode "Frog")
 )
)

(ImplicationLink (stv 1.0 1.0)
 (AndLink
  (EvaluationLink 
   (PredicateNode "chirps")
   (VariableNode "$Y")
  )
  (EvaluationLink
   (PredicateNode "sings")
   (VariableNode "$Y")
  )
 )
 (InheritanceLink
  (VariableNode "$Y")
  (ConceptNode "Canary")
 )
)

(ImplicationLink (stv 1.0 1.0)
 (InheritanceLink 
  (VariableNode "$Z")
  (ConceptNode "Frog")
 )
 (InheritanceLink
  (VariableNode "$Z")
  (ConceptNode "green")
 )
)

(ImplicationLink (stv 1.0 1.0)
 (InheritanceLink 
  (VariableNode "$A")
  (ConceptNode "Canary")
 )
 (InheritanceLink
  (VariableNode "$A")
  (ConceptNode "yellow")
 )
)

;KB
(EvaluationLink (stv 1.0 1.0)
 (PredicateNode "croaks")
 (ConceptNode "Fritz")
)

(EvaluationLink (stv 1.0 1.0)
 (PredicateNode "chirps")
 (ConceptNode "Tweety")
)

(InheritanceLink (stv 1.0 1.0)
 (ConceptNode "Tweety")
 (ConceptNode "Yello")
)

(EvaluationLink (stv 1.0 1.0)
 (PredicateNode "eats_flies")
 (ConceptNode "Tweety")
)

(EvaluationLink (stv 1.0 1.0)
 (PredicateNode "eats_flies")
 (ConceptNode "Fritz")
)

