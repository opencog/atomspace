;
; rule-engine-utils.scm
;
;;; Commentary:
;
; Handy utilities for working with the rule-engine. In particular to
; configure a rule-based system (rbs).
;
; Utilities include:
; -- ure-add-rule -- Associate a rule to a rbs
; -- ure-add-rules -- Associate  a list of rule-alias and weight pairs to a rbs
; -- ure-set-num-parameter -- Set a numeric parameter of an rbs
; -- ure-set-fuzzy-bool-parameter -- Set a fuzzy boolean parameter of an rbs
; -- ure-define-rbs -- Create a rbs that runs for a particular number of
;                      iterations.
; -- ure-get-forward-rule -- Return the forward form of a rule
; -- gt-zero-confidence -- Return TrueTV iff A's confidence is greater than 0
; -- meta-bind -- Fully apply a meta rule. Convenient for testing meta-rules
;
; If you add more utilities don't forget to add them in the
; export-rule-engine-utils function.
;
;;; Code:
; Copyright (c) 2015, OpenCog Foundation
;

(use-modules (opencog))
(use-modules (opencog query))
(use-modules (srfi srfi-1))

(define* (cog-fc rbs source #:key (vardecl (List)) (focus-set (Set)))
"
  Forward Chainer call.

  Usage: (cog-fc rbs source #:vardecl vd #:focus-set fs)

  rbs: ConceptNode representing a rulebase.

  source: Source from where to start forward chaining. If a SetLink
          then multiple sources are considered.

  vardecl: optional variable declaration of the source (in case it has
           variables)

  focus-set: optional focus-set, a SetLink with all atoms to consider
             for forward chaining
"
  (cog-mandatory-args-fc rbs source vardecl focus-set))

(define* (cog-bc rbs target #:key (vardecl (List)) (trace-as #f) (focus-set (Set)))
"
  Backward Chainer call.

  Usage: (cog-bc rbs target #:vardecl vd #:trace-as tas #:focus-set fs)

  rbs: ConceptNode representing a rulebase.

  target: Target to proof.

  tas: optional AtomSpace to record the back-inference traces.

  vd: optional variable declaration of the target (in case it has
      variables)

  fs: optional focus-set, a SetLink with all atoms to consider
      for forward chaining (NOT IMPLEMENTED).
"
  (let* ((trace-enabled (cog-atomspace? trace-as))
         (tas (if trace-enabled trace-as (cog-atomspace))))
  (cog-mandatory-args-bc rbs target vardecl trace-enabled tas focus-set)))

(define-public (ure-define-add-rule rbs rule-name rule weight)
"

  Associate a rule name and a rule content, and adds it to a rulebase
  with a given weight and returns the rule alias (DefinedSchemaNode <rule-name>).

  rbs: The ConceptNode that represents a rulebase.

  rule-name : A string that names the rule.

  rule: The BindLink that is run.

  weight: A number that is used to represent the priority of the rule.
"
    ; Didn't add type checking here b/c the ure-configuration format isn't
    ; set in stone yet. And the best place to do that is in c++ UREConfigReader
    (let ((alias (DefinedSchemaNode rule-name)))
        (DefineLink alias rule)

        (MemberLink (stv weight 1)
           alias
           rbs)

        alias
    )
)

(define-public (ure-add-rule rbs rule-alias weight)
"
  Adds a rule to a rulebase and sets its weight.

  rbs: The ConceptNode that represents a rulebase.

  rule-alias : A string that names the rule.

  weight: A number that is used to represent the priority of the rule.
"
    (MemberLink (stv weight 1) rule-alias rbs)
)

(define-public (ure-add-rules rbs rules)
"
  Given a rbs and a list of pairs (rule-alias weight) create for each rule

  MemberLink (stv weight 1)
    rule-alias
    rbs

  rbs: The ConceptNode that represents a rulebase

  rules: A list of rule-alias and weight pairs, where rule-alias is the node
         alias of a rule in a DefineLink already created.
"
  (define (expand-pair weighted-rule)
    (let* ((rule-alias (car weighted-rule))
           (weight (cadr weighted-rule)))
        (ure-add-rule rbs rule-alias weight)
    )
  )
  (for-each expand-pair rules)
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
       (extract-hypergraph del-prev-val)
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

(define-public (ure-define-rbs rbs iteration)
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

(define-public (ure-get-forward-rule rule)
"
  Given a rule return the forward form
"
  (let ((rule-type (cog-type rule)))
    (if (eq? rule-type 'ListLink) (gar rule) rule))
)

;; Very handy and frequent rule precondition.
(define (gt-zero-confidence A)
"
  Return TrueTV iff A's confidence is greater than 0
"
  (bool->tv (> (cog-stv-confidence A) 0)))

(define (meta-bind bl)
"
  Fully apply a meta rule, once for generating rules, another for
  applying the generated rules to the atomspace. Convenient for testing
  meta-rules.
"
  (let* ((rules (cog-bind bl))
         (result-sets (map cog-bind (cog-outgoing-set rules)))
         (result-lists (map cog-outgoing-set result-sets))
         (equal-lset-union (lambda (x y) (lset-union equal? x y)))
         (results (fold equal-lset-union '() result-lists)))
    (Set results)))

(define (export-rule-engine-utils)
  (export
          cog-fc
          cog-bc
          ure-define-add-rule
          ure-add-rule
          ure-add-rules
          ure-set-num-parameter
          ure-set-fuzzy-bool-parameter
          ure-define-rbs
          ure-get-forward-rule
          gt-zero-confidence
          meta-bind
          export-rule-engine-utils,
          )
)
