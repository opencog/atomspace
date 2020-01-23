
(use-modules (opencog) (opencog exec))
;
; Test data for a stack-handling bug found by Samir

(define (stv mean conf) (cog-new-stv mean conf))

; Input data
(InheritanceLink (stv 1.0 1.0)
   (PredicateNode "grab@123_Manipulation")
   (WordSenseNode "#Manipulation")
)
(InheritanceLink (stv 1.0 1.0)
   (PredicateNode "grab@123_Manipulation_Agent")
   (SemeNode "#Manipulation:Agent")
)
(InheritanceLink (stv 1.0 1.0)
   (PredicateNode "grab@123_Manipulation_Depictive")
   (SemeNode "#Manipulation:Depictive")
)
(InheritanceLink (stv 1.0 1.0)
   (PredicateNode "grab@123_Manipulation_Entity")
   (SemeNode "#Manipulation:Entity")
)

(FeatureLink (stv 1.0 1.0)
   (PredicateNode "grab@123_Manipulation")
   (PredicateNode "grab@123_Manipulation_Agent")
)
(FeatureLink (stv 1.0 1.0)
   (PredicateNode "grab@123_Manipulation")
   (PredicateNode "grab@123_Manipulation_Depictive")
)
(FeatureLink (stv 1.0 1.0)
   (PredicateNode "grab@123_Manipulation")
   (PredicateNode "grab@123_Manipulation_Entity")
)


(EvaluationLink (stv 1.0 1.0)
   (PredicateNode "grab@123_Manipulation_Agent")
   (ConceptNode "#you")
)
(EvaluationLink (stv 1.0 1.0)
   (PredicateNode "grab@123_Manipulation_Depictive")
   (ConceptNode "#grab")
)
(EvaluationLink (stv 1.0 1.0)
   (PredicateNode "grab@123_Manipulation_Entity")
   (WordInstanceNode "ball@456")
)


; The implication to be run
(define (impy)
  (Bind
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

; Running the implication should return only one answer!
; (cog-execute! (impy))

