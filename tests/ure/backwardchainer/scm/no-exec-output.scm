;; This example shows that rule conclusion no longer needs to be in a
;; dummy ExecutionOutputLink in order to enable backward chainer.
;;
;; by Shujing Ke, Feb 2018

(use-modules (opencog ure))

;; Knowledge base:

(Inheritance (stv 1.0 1.0)
   (Concept "American")
   (Concept "person"))

(Inheritance (stv 1.0 1.0)
   (Concept "cat")
   (Concept "pet"))

;; Rule base:

;; Person keeps pet
(define keep-pet-rule
 (let* ((kp (Predicate "keep-pet"))
        (like (Predicate "like"))
        (vA (Variable "$A"))
        (vX (Variable "$X"))
        (akx (Evaluation kp (List vA vX))))
  (BindLink
   (VariableList
     (TypedVariable
        vA
        (Type "ConceptNode"))
     (TypedVariable
        vX
        (Type "ConceptNode")))
   (Present
     (Inheritance
        vA
        (Concept "person"))
     (Inheritance
        vX
        (Concept "pet")))
   akx)))

(define keep-pet-rule-name
  (DefinedSchemaNode "keep-pet-rule"))
(Define keep-pet-rule-name
        keep-pet-rule)

(define Einstein-rbs (ConceptNode "Einstein-rbs"))

;; Associate the rules to the rule base
(ure-add-rule Einstein-rbs keep-pet-rule-name)

;; termination criteria parameters
(ure-set-maximum-iterations Einstein-rbs 30)

;; We can run the backward chainer to find out "Who keeps the cat?" by
;; defining a target

(define target
   (Evaluation
      (Predicate "keep-pet")
      (List
	 (Variable "$who")
	 (Concept "cat"))))

;; With the following variable declaration
(define vd
  (TypedVariable (VariableNode "$who") (TypeNode "ConceptNode")))

;;; We can now call the backward chainer as follows
;;; (cog-bc Einstein-rbs target #:vardecl vd)
;;;
;;; It should ouput:
;;; (SetLink
;;;    (EvaluationLink
;;;       (PredicateNode "keep-pet")
;;;       (ListLink
;;;          (ConceptNode "American")
;;;          (ConceptNode "cat")
;;;       )
;;;    )
;;; )
