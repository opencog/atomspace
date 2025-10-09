
(use-modules (opencog) (opencog exec))
;
; Test data for a stack-handling bug found by Samir

; Input data
(InheritanceLink
   (PredicateNode "grab@123_Manipulation")
   (WordSenseNode "#Manipulation")
)
(InheritanceLink
   (PredicateNode "grab@123_Manipulation_Agent")
   (SemeNode "#Manipulation:Agent")
)
(InheritanceLink
   (PredicateNode "grab@123_Manipulation_Depictive")
   (SemeNode "#Manipulation:Depictive")
)
(InheritanceLink
   (PredicateNode "grab@123_Manipulation_Entity")
   (SemeNode "#Manipulation:Entity")
)

(FeatureLink
   (PredicateNode "grab@123_Manipulation")
   (PredicateNode "grab@123_Manipulation_Agent")
)
(FeatureLink
   (PredicateNode "grab@123_Manipulation")
   (PredicateNode "grab@123_Manipulation_Depictive")
)
(FeatureLink
   (PredicateNode "grab@123_Manipulation")
   (PredicateNode "grab@123_Manipulation_Entity")
)


(EvaluationLink
   (PredicateNode "grab@123_Manipulation_Agent")
   (ConceptNode "#you")
)
(EvaluationLink
   (PredicateNode "grab@123_Manipulation_Depictive")
   (ConceptNode "#grab")
)
(EvaluationLink
   (PredicateNode "grab@123_Manipulation_Entity")
   (WordInstanceNode "ball@456")
)


; The implication to be run
(define (impy)
  (CollectionOf
  (Query
   (VariableList
    (TypedVariable
      (Variable "$agent")
      (TypeChoice (Type "ConceptNode") (Type "WordInstanceNode")))
    (TypedVariable (Variable "$framePred") (Type "PredicateNode"))
    (TypedVariable (Variable "$frameAgent") (Type "PredicateNode"))
    (TypedVariable (Variable "$frameDepictive") (Type "PredicateNode"))
    (TypedVariable (Variable "$frameEntity") (Type "PredicateNode"))
    (TypedVariable (Variable "$targetEntity") (Type "WordInstanceNode"))
   )
   (And
    (Inheritance (Variable "$framePred") (WordSense "#Manipulation"))
    (Inheritance (Variable "$frameAgent") (Seme "#Manipulation:Agent"))
    (Inheritance (Variable "$frameDepictive") (Seme "#Manipulation:Depictive"))
    (Inheritance (Variable "$frameEntity") (Seme "#Manipulation:Entity"))

    (Feature (Variable "$framePred") (Variable "$frameEntity"))
    (Feature (Variable "$framePred") (Variable "$frameAgent"))
    (Feature (Variable "$framePred") (Variable "$frameDepictive"))

    (Evaluation (Variable "$frameAgent") (Variable "$agent"))
    (Evaluation (Variable "$frameDepictive") (Concept "#grab"))
    (Evaluation (Variable "$frameEntity") (Variable "$targetEntity"))
   )
   (Evaluation
    (Predicate "grab")
    (List (Variable "$targetEntity") (Variable "$frameDepictive")))
  )
  )
)

; Running the implication should return only one answer!
; (cog-execute! (impy))

