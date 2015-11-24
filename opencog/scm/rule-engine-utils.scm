;
; rule-engine-utils.scm
;
;;; Commentary:
;
; Handy utilities for working with the rule-engine. In particular to
; configure a rule-based system (rbs).
;
; Utilities include:
; -- ure-add-rules -- Associate rules to an rbs
; -- ure-set-num-parameter -- Set a numeric parameter of an rbs
; -- ure-set-fuzzy-bool-parameter -- Set a fuzzy boolean parameter of an rbs
;
; If you add more utilities don't forget to add them in the
; export-rule-engine-utils function.
;
;;; Code:
; Copyright (c) 2015, OpenCog Foundation
;

(use-modules (opencog))
(use-modules (opencog query))

; Given an rbs and a list of pairs (rule weight) create for each rule
;
; MemberLink (stv weight 1)
;    rule
;    rbs
(define (ure-add-rules rbs rules)
  (define (ure-add-rule weighted-rule)
    (let ((rule (car weighted-rule))
          (weight (cadr weighted-rule)))
      (MemberLink (stv weight 1)
         rule
         rbs)))
  (for-each ure-add-rule rules)
)

; Set numerical parameters. Given an rbs, a parameter name and its
; value, create
;
; ExecutionLink
;    SchemaNode name
;    rbs
;    (NumberNode "value")
;
; It will also delete the any
;
; ExecutionLink
;    SchemaNode name
;    rbs
;    *
;
; to be sure there is ever only one value associated to that
; parameter. The value is automatically converted into string.
(define (ure-set-num-parameter rbs name value)
  (define (param-hypergraph value)
    (ExecutionLink
       (SchemaNode name)
       rbs
       value)
  )
  (let ((del-prev-val (BindLink
                          (param-hypergraph (VariableNode "__VALUE__"))
                          (DeleteLink
                             (param-hypergraph (VariableNode "__VALUE__"))))))
       ; Delete any previous value for that parameter
       (cog-bind del-prev-val)
       ; Delete pattern to not create to much junk in the atomspace
       (purge-hypergraph del-prev-val)
  )

  ; Set new value for that parameter
  (param-hypergraph (NumberNode (number->string value)))
)

; Set (fuzzy) bool parameters. Given an RBS, a parameter name and its
; value, create (or overwrite)
;
; EvaluationLink (stv value 1)
;    PredicateNode name
;    rbs
(define (ure-set-fuzzy-bool-parameter rbs name value)
  (EvaluationLink (stv value 1)
     (PredicateNode name)
     rbs)
)

(define (ure-define-rbs rbs iteration)
"
  Transforms the atom into a node that represents a rulebase and returns it.

  rbs: The ConceptNode that represents the set of rules.
  iteration: The maximum number of iteration that the rulebase should
"
    (InheritanceLink
       rbs
       (ConceptNode "RuleBase"))

    ; URE:attention-allocation isn't set b/c it isn't in use presently.
    (ure-set-num-parameter rbs "URE:maximum-iterations"  iteration)

    rbs
)

(define (ure-add-rule rbs rule-name rule weight)
"
  Adds a rule to a rulebase and sets its weight and returns the rule node.

  rbs: The ConceptNode that represents a rulebase.

  rule-name : A string that names the rule.

  rule: The BindLink that is run.

  weight: A number that is used to represent the priority of the rule.
"
    ; Didn't add type checking here b/c the ure-configuration format isn't
    ; set in stone yet. And the best place to do that is in c++ UREConfigReader
    (let ((alias (Node rule-name)))
        (DefineLink alias rule)

        (MemberLink (stv weight 1)
           alias
           rbs)

        alias
    )
)

(define (export-rule-engine-utils)
  (export ure-add-rules
          ure-set-num-parameter
          ure-set-fuzzy-bool-parameter
          export-rule-engine-utils
          ure-define-rbs
          ure-add-rule)
)
