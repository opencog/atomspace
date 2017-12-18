;
; rule-engine-utils.scm
;
;;; Commentary:
;
; Handy utilities for working with the rule-engine. In particular to
; configure a rule-based system (rbs).
;
; Utilities include:
; -- ure-add-rule -- Associate a rule to a rbs with a certain TV
; -- ure-add-rules -- Associate  a list of rule-alias and TV pairs to a rbs
; -- ure-set-num-parameter -- Set a numeric parameter of an rbs
; -- ure-set-fuzzy-bool-parameter -- Set a fuzzy boolean parameter of an rbs
; -- ure-define-rbs -- Create a rbs that runs for a particular number of
;                      iterations.
; -- ure-get-forward-rule -- Return the forward form of a rule
; -- bool->tv -- Convert #t to TRUE_TV and #f to FALSE_TV
; -- tv->bool -- Convert TRUE_TV to #t, anything else to #f
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
(use-modules (opencog exec))
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

(define* (cog-bc rbs target
                 #:key
                 (vardecl (List)) (trace-as #f) (control-as #f) (focus-set (Set)))
"
  Backward Chainer call.

  Usage: (cog-bc rbs target #:vardecl vd #:trace-as tas #:focus-set fs)

  rbs: ConceptNode representing a rulebase.

  target: Target to proof.

  vardecl: optional variable declaration of the target (in case it has
           variables)

  trace-as: optional AtomSpace to record the back-inference traces.

  control-as: optional AtomSpace storing inference control rules.

  focus-set: optional focus-set, a SetLink with all atoms to consider
             for forward chaining (NOT IMPLEMENTED).
"
  (let* ((trace-enabled (cog-atomspace? trace-as))
         (control-enabled (cog-atomspace? control-as))
         (tas (if trace-enabled trace-as (cog-atomspace)))
         (cas (if control-enabled control-as (cog-atomspace))))
  (cog-mandatory-args-bc rbs target vardecl
                         trace-enabled tas control-enabled cas focus-set)))

(define-public (ure-define-add-rule rbs rule-name rule . tv)
"

  Associate a rule name and a rule content, and adds it to a rulebase
  with a given TV and returns the rule alias (DefinedSchemaNode <rule-name>).

  rbs: The ConceptNode that represents a rulebase.

  rule-name : A string that names the rule.

  rule: The BindLink that is run.

  tv (head): Optional TV representing the probability (uncertainty included) that the rule produces a desire outcome.
"
    ; Didn't add type checking here b/c the ure-configuration format isn't
    ; set in stone yet. And the best place to do that is in c++ UREConfig
    (let ((alias (DefinedSchemaNode rule-name)))
        (DefineLink alias rule)

        (if (null? tv)
            (MemberLink
               alias
               rbs)
            (MemberLink (car tv)
               alias
               rbs))

        alias
    )
)

(define-public (ure-add-rule rbs rule-alias . tv)
"
  Adds a rule to a rulebase and sets its tv.

  rbs: The ConceptNode that represents a rulebase.

  rule-alias : A string that names the rule.

  tv (head): Optional TV representing the probability (uncertainty included) that the rule produces a desire outcome.
"
  (if (null? tv)
      (MemberLink
        rule-alias
        rbs)
      (MemberLink (car tv)
        rule-alias
        rbs))
)

(define-public (ure-add-rules rbs rules)
"
  Given a rbs and a list of pairs (rule-alias tv) create for each rule

  MemberLink tv
    rule-alias
    rbs

  rbs: The ConceptNode that represents a rulebase

  rules: A list of rule-alias, or rule-alias and tv pairs, where rule-alias
         is the node alias of a rule in a DefineLink already created.
         In case the TVs are not provided the default TV is used
"
  (define (add-rule tved-rule)
    (if (list? tved-rule)
        (let* ((rule-alias (car tved-rule))
               (tv (cadr tved-rule)))
          (ure-add-rule rbs rule-alias tv))
        (ure-add-rule rbs tved-rule)))

  (for-each add-rule rules)
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
       (cog-execute! del-prev-val)
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

(define-public (bool->tv b)
"
  Convert #t to TRUE_TV and #f to FALSE_TV
"
    (if b
        (stv 1 1)
        (stv 0 1)
    )
)

(define-public (tv->bool tv)
"
  Convert TRUE_TV to #t, anything else to #f
"
    (equal? (stv 1 1) tv))

;; Very handy and frequent rule precondition.
(define-public (gt-zero-confidence A)
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
  (let* ((rules (cog-execute! bl))
         (result-sets (map cog-execute! (cog-outgoing-set rules)))
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
          bool->tv
          tv->bool
          gt-zero-confidence
          meta-bind
          ; export-rule-engine-utils
  )
)
