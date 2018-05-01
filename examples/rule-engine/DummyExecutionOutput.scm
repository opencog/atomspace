;; This example shows that if there are multiple conclusions
;; in a rule, they need to be wrapped in one dummy ExecutionOutputLink,
;; in order to enable backward chainer.
;; by Shujing Ke, Feb 2018

;; knowledge base:

(Inheritance (stv 1.0 1.0)
   (Concept "American")
   (Concept "person"))

(Inheritance (stv 1.0 1.0)
   (Concept "cat")
   (Concept "pet"))

;; Person keeps pet
(define keep-pet-rule
 (let* (
          (kp (Predicate "keep-pet"))
          (like (Predicate "like"))
          (vA (Variable "$A"))
          (vX (Variable "$X"))
          (akx (Evaluation kp (List vA vX)))
          (alx (Evaluation like (List vA vX)))
       )
  (BindLink
   (VariableList
     (TypedVariable
        vA
        (Type "ConceptNode"))
     (TypedVariable
        vX
        (Type "ConceptNode"))
   )
   (And
     (Inheritance
        vA
        (Concept "person"))
     (Inheritance
        vX
        (Concept "pet"))
 
   )
   (And
     akx
     alx
   )
  )
 )
)

(define keep-pet-rule-name
  (DefinedSchemaNode "keep-pet-rule"))
(Define keep-pet-rule-name
        keep-pet-rule)

(define Einstein-rbs (ConceptNode "Einstein-rbs"))
(Inheritance Einstein-rbs (ConceptNode "URE"))

;; Associate the rules to the rule base 
(MemberLink (stv 1 1)
   keep-pet-rule-name
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

;;We can now call the backward chainer as follows
;;(cog-bc Einstein-rbs target #:vardecl vd)

;;; run bc with above rule, it will output empty result set.
;;; To make it work, you need to define a dummy ExecutionOutputLink
;;; to wrap the two conclusions, like below:

(define (dommyoutput-formula k m)
    (cog-set-tv! k (stv 1 1))
    (cog-set-tv! m (stv 1 1))
)

;;; Then change the conclusion in the rule above into:
(define keep-pet-rule-with-dummyoutput
 (let* (
          (kp (Predicate "keep-pet"))
          (like (Predicate "like"))
          (vA (Variable "$A"))
          (vX (Variable "$X"))
          (akx (Evaluation kp (List vA vX)))
          (alx (Evaluation like (List vA vX)))
       )
  (BindLink
   (VariableList
     (TypedVariable
        vA
        (Type "ConceptNode"))
     (TypedVariable
        vX
        (Type "ConceptNode"))
   )
   (And
     (Inheritance
        vA
        (Concept "person"))
     (Inheritance
        vX
        (Concept "pet"))
   )
   (ExecutionOutputLink
     (GroundedSchemaNode "scm: dommyoutput-formula")
     (ListLink akx alx)
   )
  )
 )
)

(define keep-pet-rule-with-dummyoutput-name
  (DefinedSchemaNode "keep-pet-rule-with-dummyoutput"))
(Define keep-pet-rule-with-dummyoutput-name
        keep-pet-rule-with-dummyoutput)

;; Now uncomment below MemberLink to add this rule into rbs
;(MemberLink (stv 1 1)
;   keep-pet-rule-with-dummyoutput-name
;   Einstein-rbs
;)

;; Then run backward chainer again:
;;(cog-bc Einstein-rbs target #:vardecl vd)

;;; It should ouput:
;;;(SetLink
;;;   (EvaluationLink (stv 1 1)
;;;      (PredicateNode "keep-pet")
;;;      (ListLink
;;;         (ConceptNode "American")
;;;         (ConceptNode "cat")
;;;      )
;;;   )
;;;)

