;; This example shows that one dummy ExecutionOutputLink,
;; and only one, is required in the conclusion
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
          (vA (Variable "$A"))
          (vX (Variable "$X"))
          (akx (Evaluation kp (List vA vX)))
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
   akx
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

;; You may expect the backward chainer to output the result that:
;; "American keep cat",
;; but the it actually output empty result.
;; The reason is the format of rule requires an dummy ExecutionOutputLink
;; to make backward chainer work.
;; Now we add a dummy ExecutionOutputLink as below:
;
;(define (dommyoutput k)
;
;    (cog-set-tv! k (stv 1 1))
;
;)

;(define keep-pet-rule
; (let* (
;          (kp (Predicate "keep-pet"))
;          (vA (Variable "$A"))
;          (vX (Variable "$X"))
;          (akx (Evaluation kp (List vA vX)))
;       )
;  (BindLink
;   (VariableList
;     (TypedVariable
;        vA
;        (Type "ConceptNode"))
;     (TypedVariable
;        vX
;        (Type "ConceptNode"))
;   )
;   (And
;     (Inheritance
;        vA
;        (Concept "person"))
;     (Inheritance
;        vX
;        (Concept "pet"))
; 
;   )
;   (ExecutionOutputLink
;       (GroundedSchemaNode "scm: dommyoutput")
;       akx
;     )
;  )
; )
;)

;;; Now run the backward chainer again:
;;(cog-bc Einstein-rbs target #:vardecl vd)
;
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

;;; Further more, only one conclusion is allowed in one rule.
;;; If you have multiple ExecutionOutputLinks in the conclusion,
;;; and link them with a AndLink, it won't work either.
;;; Like below example won't work.
;(define keep-pet-rule
; (let* (
;          (kp (Predicate "keep-pet"))
;          (like (Predicate "like"))
;          (vA (Variable "$A"))
;          (vX (Variable "$X"))
;          (akx (Evaluation kp (List vA vX)))
;          (alx (Evaluation like (List vA vX)))
;       )
;  (BindLink
;   (VariableList
;     (TypedVariable
;        vA
;        (Type "ConceptNode"))
;     (TypedVariable
;        vX
;        (Type "ConceptNode"))
;   )
;   (And
;     (Inheritance
;        vA
;        (Concept "person"))
;     (Inheritance
;        vX
;        (Concept "pet"))
; 
;   )
;   (And
;      (ExecutionOutputLink
;          (GroundedSchemaNode "scm: dommyoutput")
;          akx
;      )
;      (ExecutionOutputLink
;          (GroundedSchemaNode "scm: dommyoutput")
;          alx
;      )
;   )
;  )
; )
;)

;;; To make it work, you need to define a dummy function
;;; that takes two args, like below:
;(define (dommyoutput2 k m)
;    (cog-set-tv! k (stv 1 1))
;    (cog-set-tv! m (stv 1 1))
;)

;;; Then change the conclusion in the rule above into:
;(ExecutionOutputLink
;   (GroundedSchemaNode "scm: dommyoutput2")
;   akx alx
;)
