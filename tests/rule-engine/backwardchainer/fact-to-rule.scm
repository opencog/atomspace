;; This toy puzzle is to give an example that 
;; sometimes a fact needed to be represented as a rule
;; in order to enable backward chainer.
;; In this example, the fact "American is person" is rewritten 
;; as a rule that "If ($A == American) -> $A is a person".
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
   

     (ExecutionOutputLink
       (GroundedSchemaNode "scm: dommyoutput")
       akx
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

;; You may expect the backward chainer to output the result that:
;; "American keep cat",
;; but the it actually output empty result.
;; The reason is it lacks of a rule to imply that American is a person.
;; Note that the known fact that:
;;(Inheritance (stv 1.0 1.0)
;;   (Concept "American")
;;   (Concept "person"))
;; is a fact, not a rule.
;; Let's simulate the backward reasoning process of this problem:
;; Target 0:  ($who keep cat)

;; STEP 1: Search for a rule to achieve target 0:
;;         "keep-pet-rule" is found. Variable bound:
;;         If ( $who is person ) and ( cat is pet ), then ( $who keep cat ).
;;         Two preconditions of this rule become the next level targets:
;; Target 1: Target 1.1:   ( $who is person )
;;           Target 1.2:   ( cat is pet )   

;; STEP 2: Target 1.2 is already satisifed in the knowledge base.
;;         So Target 1.1 is selected as the current target.
;;         Search for rule to achieve Target 1.1, no rule found.
;;         So basically an explicit rule is lacked, like:
;;         if ($A == American ), then ( $A is person)
;; Therefore the truth "American is a person" needs to be converted into such
;; a rule to make the backward chainer work. 

;; now try to clear the previous running result with clear, and then reload the file,
;; then add the below American-is-person-rule:
(define American-is-person-rule
 (let* (
          (vA (Variable "$A"))
          (person (Concept "person"))   
          (American (Concept "American"))   
          (A-is-person (Inheritance vA person)) 
       )

  (BindLink
   (TypedVariable
        vA
        (Type "ConceptNode")
   ) 

   (Equal vA American) 

   (ExecutionOutputLink
       (GroundedSchemaNode "scm: dommyoutput")
       A-is-person
   )
   
  )
 )
)


(define American-is-person-rule-name
  (DefinedSchemaNode "American-is-person-rule"))
(Define American-is-person-rule-name
        American-is-person-rule)


(define (dommyoutput k)

    (cog-set-tv! k (stv 1 1))

)

;; Add it to the rule base
(MemberLink (stv 1 1)
   American-is-person-rule-name
   Einstein-rbs
)

;; Now run the backward chainer again:
;(cog-bc Einstein-rbs target #:vardecl vd)

;; It should ouput:
;;(SetLink
;;   (EvaluationLink (stv 1 1)
;;      (PredicateNode "keep-pet")
;;      (ListLink
;;         (ConceptNode "American")
;;         (ConceptNode "cat")
;;      )
;;   )
;;)

; Therefore, in order to make sure bc works, 
;; you can write meta rules to auto convert facts into rules.
