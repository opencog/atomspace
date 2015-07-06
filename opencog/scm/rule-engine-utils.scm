;
; rule-engine-utils.scm
;
;;; Commentary:
;
; Handy utilities for working with the rule-engine. In particular to
; configure a rule-based system (rbs).
;
; Utilities include:
; -- ure-add-weighed-rules -- Associate rules to an rbs
; -- ure-set-num-parameter -- Set a numeric parameter of an rbs
; -- ure-set-fuzzy-bool-parameter -- Set a fuzzy boolean parameter of an rbs
;
;;; Code:
; Copyright (c) 2015, OpenCog Foundation
;

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
  ; Delete any previous parameter
  (cog-bind (BindLink
               (ExecutionLink
                  (SchemaNode name)
                  rbs
                  (VariableNode "__VALUE__"))
               (DeleteLink
                  (ExecutionLink
                     (SchemaNode name)
                     rbs
                     (VariableNode "__VALUE__")))))

  ; Set new value for that parameter
  (ExecutionLink
     (SchemaNode name)
     rbs
     (NumberNode (number->string value)))
)

; Set (fuzzy) bool parameters. Given an RBS, a parameter name and its
; value, create (or overwrite)
;
; EvaluationLink (stv value 1)
;    PredicateNode name
;    rbs
(define (ure-set-fuzzy-bool-parameter rbs name value)
  (EvaluationLink (stv value 1)
     (SchemaNode name)
     rbs)
)
