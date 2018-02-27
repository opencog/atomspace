;; This is a toy puzzle to give an example of negation conflict test.
;; There are two persons: American and German.
;; There are two pets: cat and dog.
;; Each person keeps one different pet.
;; The German doesn't keep the dog.
;; Question: who keeps the cat?
;; By shujing Ke, Feb 2018

(Inheritance (stv 1.0 1.0)
   (Concept "American")
   (Concept "person"))

(Inheritance (stv 1.0 1.0)
   (Concept "German")
   (Concept "person"))

(Inheritance (stv 1.0 1.0)
   (Concept "cat")
   (Concept "pet"))

(Inheritance (stv 1.0 1.0)
   (Concept "dog")
   (Concept "pet"))



;; The German doesn't keep the dog.
;; This truth is represented by the TV (stv 0.0 1.0)
;; Note that it won't work if you just wrap the EvaluationLink with a NotLink,
;; Because NotLink is a virtual Link, which is only executed in runtime.
;; NotLinks should not be used directly to represent a truth.
(Evaluation (stv 0.0 1.0)
   (Predicate "keep-pet")
   (List
   	(Concept "German")
        (Concept "dog")))



(define (evaluation-absent predicate A B )
    (bool->tv (null? (cog-link "EvaluationLink" predicate (List A B))) )
)


;; If A keeps a pet x, B is different FROM A, 
;; and there exists some other pet, Y, 
;; that is different from X, then B keeps Y
(define keep-different-pet-rule
 (let* (
          (kp (Predicate "keep-pet"))
          (vA (Variable "$A"))
          (vB (Variable "$B"))
          (vX (Variable "$X"))
          (vY (Variable "$Y"))
          (akx (Evaluation kp (List vA vX)))
          (bky (Evaluation kp (List vB vY)))
       )

  (BindLink
   (VariableList
     (TypedVariable
        vA
        (Type "ConceptNode"))
     (TypedVariable
        vB
        (Type "ConceptNode"))
     (TypedVariable
        vX
        (Type "ConceptNode"))
     (TypedVariable
        vY
        (Type "ConceptNode"))
   )
   (And
     (Inheritance
        vA
        (Concept "person"))
     (Inheritance
        vB
        (Concept "person"))  
     (Inheritance
        vX
        (Concept "pet"))
     (Inheritance
        vY
        (Concept "pet"))
     (NotLink
        (EqualLink
           vA
           vB
        ))
     (NotLink
        (EqualLink
           vX
           vY
        ))

     ;; test if the conclusion will conflict with the known truth:
     ;; it's fine if the conclusion doesn't exist in the knowledge base;
     ;; but if it already exist and the truth value is not true, 
     ;; then this conclusion conflict with the know truth, so that it 
     ;; should not be selected.
     (Or
        (EvaluationLink
	   (GroundedPredicateNode "scm: evaluation-absent")
	   (ListLink kp vA vX)
        )
        (EvaluationLink
	   (GroundedPredicateNode "scm: absolutely-true")
	   (ListLink akx)
        )       
 
     )

     (Or
        (EvaluationLink
	   (GroundedPredicateNode "scm: evaluation-absent")
	   (ListLink kp vB vY)
        )
        (EvaluationLink
	   (GroundedPredicateNode "scm: absolutely-true")
	   (ListLink bky)
        )       
 
     )
 
   )      
   
   (ExecutionOutputLink
     (GroundedSchemaNode "scm: keep-different-pet-formula")
     (ListLink akx bky)
   )
   
  )
 )
)

(define (keep-different-pet-formula akx bky)

    (cog-set-tv! akx (stv 1 1))
    (cog-set-tv! bky (stv 1 1))

)

(define keep-different-pet-rule-name
  (DefinedSchemaNode "keep-different-pet-rule"))
(Define keep-different-pet-rule-name
        keep-different-pet-rule)




;; below is the ure config for this example:

(define Einstein-rbs (ConceptNode "Einstein-rbs"))
(Inheritance Einstein-rbs (ConceptNode "URE"))


;; Associate the rules to the rule base 
(MemberLink (stv 1 1)
   keep-different-pet-rule-name
   Einstein-rbs
)

;; termination criteria parameters
(ExecutionLink
   (SchemaNode "URE:maximum-iterations")
   Einstein-rbs
   (NumberNode "30")
)

;; Attention allocation (set the TV strength to 0 to disable it, 1 to
;; enable it)
(EvaluationLink (stv 0 1)
   (PredicateNode "URE:attention-allocation")
   Einstein-rbs
)



;;We can run the backward chainer to find out "Who keeps the cat?" by
;;defining a target

(define target
   (Evaluation
      (Predicate "keep-pet")
      (List
	 (Variable "$who")
	 (Concept "cat")))
)


;;with the following variable declaration

(define vd
  (TypedVariable (VariableNode "$who") (TypeNode "ConceptNode"))
)

(define source
  (Inheritance (stv 1.0 1.0)
   (Concept "American")
   (Concept "person"))
)

;; Forward chainer:
;; (cog-fc Einstein-rbs (Variable "$x")) 
;; (cog-fc Einstein-rbs source)
;; The expected result is:
;(SetLink
;   (EvaluationLink (stv 1 1)
;      (PredicateNode "keep-pet")
;      (ListLink
;         (ConceptNode "German")
;         (ConceptNode "cat")
;      )
;   )
;   (EvaluationLink (stv 1 1)
;      (PredicateNode "keep-pet")
;      (ListLink
;         (ConceptNode "American")
;         (ConceptNode "dog")
;      )
;   )
;)


;;But the backward chainer won't work:
;;(cog-bc Einstein-rbs target #:vardecl vd)
;; It will output empty result, because it lack of rules.
;; Please check out FactToRule.scm for the reason.


