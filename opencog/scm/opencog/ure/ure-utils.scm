;;
;; ure-utils.scm
;;
;;;; Commentary:
;;
;; Handy utilities for working with the ure. In particular to
;; configure a rule-based system (rbs).
;;
;; Utilities include:
;; -- ure-add-rule -- Associate a rule to a rbs with a certain TV
;; -- ure-add-rule-by-name -- Associate a rule (by name) to a rbs with a certain TV
;; -- ure-add-rules -- Associate  a list of rule-alias and TV pairs to a rbs
;; -- ure-add-rules-by-names -- Associate a list of rules (by names) and TV pairs to a rbs
;; -- ure-rm-rule -- Remove rule from a rbs
;; -- ure-rm-rule-by-name -- Remove rule from a rbs given the name of its alias
;; -- ure-rm-rules -- Remove rules from a rbs
;; -- ure-rm-rules-by-names -- Remove rules from a rbs given the names of its aliases
;; -- ure-weighted-rules -- List all weighted rules of a given rule base
;; -- ure-set-num-parameter -- Set a numeric parameter of an rbs
;; -- ure-set-fuzzy-bool-parameter -- Set a fuzzy boolean parameter of an rbs
;; -- ure-set-attention-allocation -- Set the URE:attention-allocation parameter
;; -- ure-set-maximum-iterations -- Set the URE:maximum-iterations parameter
;; -- ure-set-complexity-penalty -- Set the URE:complexity-penalty parameter
;; -- ure-set-jobs -- Set the URE:jobs parameter
;; -- ure-set-fc-retry-exhausted-sources -- Set the URE:FC:retry-exhausted-sources parameter
;; -- ure-set-bc-maximum-bit-size -- Set the URE:BC:maximum-bit-size
;; -- ure-set-bc-mm-complexity-penalty -- Set the URE:BC:MM:complexity-penalty
;; -- ure-set-bc-mm-compressiveness -- Set the URE:BC:MM:compressiveness
;; -- ure-define-rbs -- Create a rbs that runs for a particular number of
;;                      iterations.
;; -- ure-logger-set-level! -- Set level of the URE logger
;; -- ure-logger-get-level -- get level of the URE logger
;; -- ure-logger-set-filename! -- set filename of the URE logger
;; -- ure-logger-get-filename -- get filename of the URE logger
;; -- ure-logger-set-stdout! -- set stdout flag of the URE logger
;; -- ure-logger-set-sync! -- set sync flag of the URE logger
;; -- ure-logger-set-timestamp! -- set timestamp falg of the URE logger
;; -- ure-logger-error-enabled? -- check that the log level of the URE logger is error enabled
;; -- ure-logger-warn-enabled? -- check that the log level of the URE logger is warn enabled
;; -- ure-logger-info-enabled? -- check that the log level of the URE logger is info enabled
;; -- ure-logger-debug-enabled? -- check that the log level of the URE logger is debug enabled
;; -- ure-logger-fine-enabled? -- check that the log level of the URE logger is fine enabled
;; -- ure-logger-error -- log at error level of the URE logger
;; -- ure-logger-warn -- log at warn level of the URE logger
;; -- ure-logger-info -- log at info level of the URE logger
;; -- ure-logger-debug -- log at debug level of the URE logger
;; -- ure-logger-fine -- log at fine level of the URE logger
;; -- ure-logger-flush -- flush the URE logger
;; -- bool->tv -- Convert #t to TRUE_TV and #f to FALSE_TV
;; -- tv->bool -- Convert TRUE_TV to #t, anything else to #f
;; -- atom->number -- Convert NumberNode into its corresponding number
;; -- gt-zero-confidence -- Return TrueTV iff A's confidence is greater than 0
;; -- absolutely-true -- Return TrueTV iff A's TV is TrueTV
;; -- meta-bind -- Fully apply a meta rule. Convenient for testing meta-rules
;; -- gen-variable -- Generate VariableNode with certain prefix and index
;; -- gen-variables -- Generate VariableNodes with certain prefix and indexes
;; -- gen-rand-variable -- Generate random VariableNode
;; -- gen-rand-variables -- Generate random VariableNodes
;; -- cog-new-flattened-link -- Create flattened link TODO: remove cog- prefix
;; -- simple-forward-step -- Simple forward step over a focus-set
;; -- simple-forward-chain -- Iterations of simple foward steps over a focus-set
;;
;; If you add more utilities don't forget to add them in the
;; export-ure-utils function.
;;
;;;; Code:
;; Copyright (c) 2015, OpenCog Foundation
;;

(use-modules (opencog))
(use-modules (opencog exec))
(use-modules (opencog logger))
(use-modules (srfi srfi-1))
(use-modules (ice-9 receive))

(define* (cog-fc rbs source
                 #:key
                 (vardecl (List))
                 (focus-set (Set))
                 (attention-allocation *unspecified*)
                 (maximum-iterations *unspecified*)
                 (complexity-penalty *unspecified*)
                 (jobs *unspecified*)
                 (fc-retry-exhausted-sources *unspecified*))
"
  Forward Chainer call.

  Usage: (cog-fc rbs source
                 #:vardecl vd
                 #:focus-set fs
                 #:attention-allocation aa
                 #:maximum-iterations mi
                 #:complexity-penalty cp
                 #:jobs jb
                 #:fc-retry-exhausted-sources res)

  rbs: ConceptNode representing a rulebase.

  source: Source from where to start forward chaining. If a SetLink
          then multiple sources are considered.

  vd: [optional] Variable declaration of the source (in case it has
      variables).

  fs: [optional] Focus set, a SetLink with all atoms to consider for
      forward chaining.

  aa: [optional] Whether the atoms involved with the
      inference are restricted to the attentional focus.

  mi: [optional] Maximum number of iterations.

  cp: [optional] Complexity penalty. Controls breadth vs depth search.
      A high value means more breadth. A value of 0 means an equilibrium
      between breadth and depth. A negative value means more depth.
      Possible range is (-inf, +inf) but it's rarely necessary in practice
      to go outside of [-10, 10].

  jb: [optional] Number of jobs to run in parallel. Can speed up reasoning,
      note that this may alter the results, especially for the forward chainer
      as the output of a rule application may depend on the output of the other
      rules.

  res: [optional] Whether exhausted sources should be retried. A source is
       exhausted if all its valid rules (so that at least one rule premise
       unifies with the source) have been applied to it. Given that the
       forward chainer results are added to the kb atomspace during forward
       chaining, the same source may yield different results as time goes,
       thus this option.

  Note that optional arguments do not have defaults! That is in order not
  to overwrite existing parameters set by ure-set-maximum-iterations and such.
  If these parameters were not set at all, then the rule engine itself selects
  defaults. These defaults will be logged in the log file.
"
  ;; Set optional parameters
  (if (not (unspecified? attention-allocation))
      (ure-set-attention-allocation rbs attention-allocation))
  (if (not (unspecified? maximum-iterations))
      (ure-set-maximum-iterations rbs maximum-iterations))
  (if (not (unspecified? complexity-penalty))
      (ure-set-complexity-penalty rbs complexity-penalty))
  (if (not (unspecified? jobs))
      (ure-set-jobs rbs jobs))
  (if (not (unspecified? fc-retry-exhausted-sources))
      (ure-set-fc-retry-exhausted-sources rbs fc-retry-exhausted-sources))

  ;; Call the forward chainer
  (cog-mandatory-args-fc rbs source vardecl focus-set))

(define* (cog-bc rbs target
                 #:key
                 (vardecl (List))
                 (trace-as #f)
                 (control-as #f)
                 (focus-set (Set))
                 (attention-allocation *unspecified*)
                 (maximum-iterations *unspecified*)
                 (complexity-penalty *unspecified*)
                 (jobs *unspecified*)
                 (bc-maximum-bit-size *unspecified*)
                 (bc-mm-complexity-penalty *unspecified*)
                 (bc-mm-compressiveness *unspecified*))
"
  Backward Chainer call.

  Usage: (cog-bc rbs target
                 #:vardecl vd
                 #:trace-as tas
                 #:control-as cas
                 #:focus-set fs
                 #:attention-allocation aa
                 #:maximum-iterations mi
                 #:complexity-penalty cp
                 #:bc-maximum-bit-size mbs
                 #:bc-mm-complexity-penalty mcp
                 #:bc-mm-compressiveness mc)

  rbs: ConceptNode representing a rulebase.

  target: Target to proof.

  vd: [optional] Variable declaration of the target (in case it
      has variables).

  tas: [optional] AtomSpace to record the back-inference traces.

  cas: [optional] AtomSpace storing inference control rules.

  fs: [optional] focus-set, a SetLink with all atoms to
      consider for forward chaining (Not Implemented).

  aa: [optional] Whether the atoms involved with the
      inference are restricted to the attentional focus.

  mi: [optional] Maximum number of iterations.

  cp: [optional] Complexity penalty. Controls breadth vs depth search.
      A high value means more breadth. A value of 0 means an equilibrium
      between breadth and depth. A negative value means more depth.
      Possible range is (-inf, +inf) but it's rarely necessary in practice
      to go outside of [-10, 10].

  jb: [optional] Number of jobs to run in parallel. Can speed up reasoning,
      note that this may alter the results, especially for the forward chainer
      as the output of a rule application may depend on the output of the other
      rules.

  mbs: [optional] Maximum size of the inference tree pool to evolve.

  mcp: [optional] Complexity penalty applied to the control rules during
       Bayesian Model Averaging.

  mc: [optional] Compressiveness parameter for partial control rules
      (how well a control rule can explain data outside of its context).

  Note that optional arguments do not have defaults! That is in order not
  to overwrite existing parameters set by ure-set-maximum-iterations and such.
  If these parameters were not set at all, then the rule engine itself selects
  defaults. These defaults will be logged in the log file.
"
  ;; Set optional parameters
  (if (not (unspecified? attention-allocation))
      (ure-set-attention-allocation rbs attention-allocation))
  (if (not (unspecified? maximum-iterations))
      (ure-set-maximum-iterations rbs maximum-iterations))
  (if (not (unspecified? complexity-penalty))
      (ure-set-complexity-penalty rbs complexity-penalty))
  (if (not (unspecified? jobs))
      (ure-set-jobs rbs jobs))
  (if (not (unspecified? bc-maximum-bit-size))
      (ure-set-bc-maximum-bit-size rbs bc-maximum-bit-size))
  (if (not (unspecified? bc-mm-complexity-penalty))
      (ure-set-bc-mm-complexity-penalty rbs bc-mm-complexity-penalty))
  (if (not (unspecified? bc-mm-compressiveness))
      (ure-set-bc-mm-compressiveness rbs bc-mm-compressiveness))

  ;; Defined optional atomspaces and call the backward chainer
  (let* ((trace-enabled (cog-atomspace? trace-as))
         (control-enabled (cog-atomspace? control-as))
         (tas (if trace-enabled trace-as (cog-atomspace)))
         (cas (if control-enabled control-as (cog-atomspace))))
    (cog-mandatory-args-bc rbs target vardecl
                           trace-enabled tas control-enabled cas focus-set)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; URE Configuration Helpers ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (ure-define-add-rule rbs rule-name rule . tv)
"
  Associate a rule name and a rule content, and adds it to a rulebase
  with a given TV and returns the rule alias (DefinedSchemaNode <rule-name>).
  That is add the following

  MemberLink <tv>
    (DefinedSchemaNode <rule-name>)
    rbs

  MemberLink and DefinedSchemaNode are added in the same atomspace as rbs.

  rbs: The ConceptNode that represents a rulebase.

  rule-name : A string that names the rule.

  rule: The BindLink that is run.

  tv: [optional] TV representing the probability and its confidence
      that the rule produces a desire outcome.
"
    ;; Switch to rbs atomspace
    (define current-as (cog-set-atomspace! (cog-as rbs)))

    (define (mk-member alias tv) (if (null? tv)
                                     (MemberLink alias rbs)
                                     (MemberLink tv alias rbs)))

    ;; Didn't add type checking here b/c the ure-configuration format isn't
    ;; set in stone yet. And the best place to do that is in c++ UREConfig
    (let ((alias (DefinedSchemaNode rule-name)))
        (DefineLink alias rule)
        (mk-member alias tv)
        (cog-set-atomspace! current-as)
        alias
    )
)

(define-public (ure-add-rule rbs rule-alias . tv)
"
  Usage: (ure-add-rule rbs rule-alias . tv)

  Given

  rbs: a ConceptNode that represents a rulebase,

  rule-alias : a DefinedSchemeNode of the rule,

  tv (head): optional TV representing the probability (including
             confidence) that the rule produces a desire outcome,

  adds a rule to a rulebase and sets its tv.

"
  (if (null? tv)
      (MemberLink rule-alias rbs)
      (MemberLink (car tv) rule-alias rbs)))

(define-public (ure-add-rule-by-name rbs rule-name . tv)
"
  Given

  rbs: The ConceptNode that represents a rulebase,

  rule-name : a string of the rule name,

  tv (head): Optional TV representing the probability (including
             confidence) that the rule produces a desire outcome,

  adds a rule to a rulebase and sets its tv, that is

  Member
    DefinedSchemaNode rule-name
    rbs

  The rule is added in the atomspace of rbs, if different from the
  current one.
"
  ;; Switch to rbs atomspace
  (define current-as (cog-set-atomspace! (cog-as rbs)))
  (define rule-alias (DefinedSchemaNode rule-name))
  (let ((member (apply ure-add-rule (cons rbs (cons rule-alias tv)))))
    (cog-set-atomspace! current-as)
    member))

(define-public (ure-add-rules rbs rules)
"
  Given

  rbs: a ConceptNode that represents a rulebase,

  rules: A list of rule-alias, or rule-alias and tv pairs, represented as

         (list rule-alias tv)

         or

         (cons rule-alias tv)

         where rule-alias is the node alias of a rule in a DefineLink
         already created. In case the TVs are not provided the default
         TV is used.

  create for each rule

  MemberLink tv
    rule-alias
    rbs
"
  (define (add-rule tved-rule)
    (if (pair? tved-rule)
        (let* ((rule-alias (car tved-rule))
               (tv (if (list? tved-rule)
                       (cadr tved-rule)
                       (cdr tved-rule))))
          (ure-add-rule rbs rule-alias tv))
        (ure-add-rule rbs tved-rule)))

  (for-each add-rule rules)
)

(define-public (ure-add-rules-by-names rbs rules)
"
  Given

  rbs: a ConceptNode that represents a rulebase,

  rules: A list of rule-names, or rule-name and tv pairs, represented as

         (list rule-name tv)

         or

         (cons rule-name tv)

         where rule-name is the rule name of DefinedSchemaNode rule
         alias in a already created DefineLink. In case the TVs are not
         provided the default TV is used.

  create for each rule

  MemberLink tv
    DefinedSchemaNode rule-name
    rbs

  The rules are added in the atomspace of rbs, if different from the
  current one.
"
  (define current-as (cog-set-atomspace! (cog-as rbs)))
  (define (add-rule-by-name-in-rbs rule-name)
    (ure-add-rule-by-name rbs rule-name))
  (for-each add-rule-by-name-in-rbs rules)
  (cog-set-atomspace! current-as)
  *unspecified*)

(define-public (ure-rm-rule rbs rule-alias)
"
  Given a rule-base and rule alias, remove the rule from the rule-base

  That is remove from the rule-base atomspace

  Member
    <rule-alias>
    <rbs>
"
  (define current-as (cog-set-atomspace! (cog-as rbs)))
  (define member (MemberLink rule-alias rbs))
  (cog-delete member)
  (cog-set-atomspace! current-as)
  *unspecified*
)

(define-public (ure-rm-rule-by-name rbs rule-name)
"
  Like ure-rm-rule but provide the name of the DefinedSchemaNode
  instead of the atom (called alias)
"
  (define current-as (cog-set-atomspace! (cog-as rbs)))
  (define rule-alias (DefinedSchemaNode rule-name))
  (ure-rm-rule rbs rule-alias)
  (cog-set-atomspace! current-as)
  *unspecified*
)

(define-public (ure-rm-rules rbs rule-aliases)
"
  Given a list of rule aliases remove all of them (call ure-rm-rule
  over all of them)
"
  (define rm-rule-from-rbs (lambda (rule-alias) (ure-rm-rule rbs rule-alias)))
  (for-each rm-rule-from-rbs rule-aliases)
)

(define-public (ure-rm-rules-by-names rbs rule-names)
"
  Like ure-rm-rules but provide the names of the DefinedSchemaNode
  to remove instead of the atoms (called aliases)
"
  (define current-as (cog-set-atomspace! (cog-as rbs)))
  (define rule-aliases (map DefinedSchemaNode rule-names))
  (ure-rm-rules rbs rule-aliases)
  (cog-set-atomspace! current-as)
  *unspecified*
)

(define-public (ure-weighted-rules rbs)
"
  List all weighted rules of rbs, as follow

  ((tv-1 . rule-1)
   ...
   (tv-n . rule-n))
"
  (define current-as (cog-set-atomspace! (cog-as rbs)))
  (let* ((get-weighted-rule (lambda (x) (cons (cog-tv (MemberLink x rbs)) x)))
         (weighted-rules (cog-map-chase-link 'MemberLink
                                             'DefinedSchemaNode
                                             get-weighted-rule
                                             rbs))
         ;; Remove extraneous list (no idea why it is here)
         (clean-weighted-rules (map car weighted-rules)))

    (cog-set-atomspace! current-as)
    clean-weighted-rules))

(define (ure-set-num-parameter rbs name value)
"
  Set numerical parameters. Given an rbs, a parameter name and its
  value, create (in the same atomspace where rbs lives)

  ExecutionLink
     SchemaNode name
     rbs
     NumberNode value

  If a value already exists it first delete it to make sure there is
  only one value associated to that parameter and rule-base.
"
  ;; Switch to rbs atomspace
  (define current-as (cog-set-atomspace! (cog-as rbs)))

  (define (param-execution atom)
    (ExecutionLink
       (SchemaNode name)
       rbs
       atom)
  )

  ;; Delete existing value if any
  (let* ((var (VariableNode "__VALUE__"))
         (exec-var (param-execution var))
         (del-prev-val (BindLink
                         exec-var
                         (DeleteLink exec-var))))
    ;; Delete any previous value for that parameter
    (cog-execute! del-prev-val)
    ;; Delete pattern to not create to much junk in the atomspace
    (cog-delete del-prev-val)
    (cog-delete (DeleteLink exec-var))
    (cog-delete exec-var)
    (cog-delete var)
  )

  ; Set new value for that parameter, switch back to current-as and
  ; return new value.
  (let ((new-param-exec (param-execution (NumberNode value))))
    (cog-set-atomspace! current-as)
    new-param-exec))

(define (ure-set-fuzzy-bool-parameter rbs name value)
"
  Set (fuzzy) bool parameters. Given an RBS, a parameter name and its
  value, create or overwrite (in the same atomspace where rbs lives)

  EvaluationLink (stv value 1)
     PredicateNode name
     rbs

  If the provided value is a boolean, then it is automatically
  converted into tv.
"
  ;; Switch to rbs atomspace
  (define current-as (cog-set-atomspace! (cog-as rbs)))

  (define (param-evaluation tv)
    (EvaluationLink tv
       (PredicateNode name)
       rbs)
  )

  ;; Set new value for that parameter, switch back to
  ;; current-as and return new value.
  (let* ((tv (if (number? value) (stv value 1) (bool->tv value)))
         (new-param-eval (param-evaluation tv)))
    (cog-set-atomspace! current-as)
    new-param-eval))

(define (ure-set-attention-allocation rbs value)
"
  Set the URE:attention-allocation parameter of a given RBS

  EvaluationLink (stv (if value 1 0) 1)
    SchemaNode \"URE:attention-allocation\"
    rbs
    NumberNode value

  where value is either #t or #f.

  Delete any previous one if exists.
"
  (ure-set-fuzzy-bool-parameter rbs "URE:attention-allocation" value))

(define (ure-set-maximum-iterations rbs value)
"
  Set the URE:maximum-iterations parameter of a given RBS

  ExecutionLink
    SchemaNode \"URE:maximum-iterations\"
    rbs
    NumberNode value

  Delete any previous one if exists.
"
  (ure-set-num-parameter rbs "URE:maximum-iterations" value))

(define (ure-set-complexity-penalty rbs value)
"
  Set the URE:complexity-penalty parameter of a given RBS

  ExecutionLink
    SchemaNode \"URE:complexity-penalty\"
    rbs
    NumberNode value

  Delete any previous one if exists.
"
  (ure-set-num-parameter rbs "URE:complexity-penalty" value))

(define (ure-set-jobs rbs value)
"
  Set the URE:jobs parameter of a given RBS

  ExecutionLink
    SchemaNode \"URE:jobs\"
    rbs
    NumberNode value

  Delete any previous one if exists.
"
  (ure-set-num-parameter rbs "URE:jobs" value))

(define (ure-set-fc-retry-exhausted-sources rbs value)
"
  Set the URE:FC:retry-exhausted-sources parameter of a given RBS

  EvaluationLink (stv value 1)
    PredicateNode \"URE:FC:retry-exhausted-sources\"
    rbs

  If the provided value is a boolean, then it is automatically
  converted into tv.
"
  (ure-set-fuzzy-bool-parameter rbs "URE:FC:retry-exhausted-sources" value))

(define (ure-set-bc-maximum-bit-size rbs value)
"
  Set the URE:BC:maximum-bit-size parameter of a given RBS

  ExecutionLink
    SchemaNode \"URE:BC:maximum-bit-size\"
    rbs
    NumberNode value

  Delete any previous one if exists.
"
  (ure-set-num-parameter rbs "URE:BC:maximum-bit-size" value))

(define (ure-set-bc-mm-complexity-penalty rbs value)
"
  Set the URE:BC:MM:complexity-penalty parameter of a given RBS

  ExecutionLink
    SchemaNode \"URE:BC:MM:complexity-penalty\"
    rbs
    NumberNode value

  Delete any previous one if exists.
"
  (ure-set-num-parameter rbs "URE:BC:MM:complexity-penalty" value))

(define (ure-set-bc-mm-compressiveness rbs value)
"
  Set the URE:BC:MM:compressiveness parameter of a given RBS

  ExecutionLink
    SchemaNode \"URE:BC:MM:compressiveness\"
    rbs
    NumberNode value

  Delete any previous one if exists.
"
  (ure-set-num-parameter rbs "URE:BC:MM:compressiveness" value))

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

;;;;;;;;;;;;;;;;
;; URE Logger ;;
;;;;;;;;;;;;;;;;

(define (ure-logger-set-level! l) (cog-logger-set-level! (cog-ure-logger) l))
(define (ure-logger-get-level) (cog-logger-get-level (cog-ure-logger)))
(define (ure-logger-set-filename! filename) (cog-logger-set-filename! (cog-ure-logger) filename))
(define (ure-logger-get-filename) (cog-logger-get-filename (cog-ure-logger)))
(define (ure-logger-set-stdout! enable) (cog-logger-set-stdout! (cog-ure-logger) enable))
(define (ure-logger-set-sync! enable) (cog-logger-set-sync! (cog-ure-logger) enable))
(define (ure-logger-set-timestamp! enable) (cog-logger-set-timestamp! (cog-ure-logger) enable))
(define (ure-logger-error-enabled?) (cog-logger-error-enabled? (cog-ure-logger)))
(define (ure-logger-warn-enabled?) (cog-logger-warn-enabled? (cog-ure-logger)))
(define (ure-logger-info-enabled?) (cog-logger-info-enabled? (cog-ure-logger)))
(define (ure-logger-debug-enabled?) (cog-logger-debug-enabled? (cog-ure-logger)))
(define (ure-logger-fine-enabled?) (cog-logger-fine-enabled? (cog-ure-logger)))
(define (ure-logger-error . args) (apply cog-logger-error (cons (cog-ure-logger) args)))
(define (ure-logger-warn . args) (apply cog-logger-warn (cons (cog-ure-logger) args)))
(define (ure-logger-info . args) (apply cog-logger-info (cons (cog-ure-logger) args)))
(define (ure-logger-debug . args) (apply cog-logger-debug (cons (cog-ure-logger) args)))
(define (ure-logger-fine . args) (apply cog-logger-fine (cons (cog-ure-logger) args)))
(define (ure-logger-flush) (cog-logger-flush (cog-ure-logger)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers for Implementing URE Rules ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (bool->tv b)
"
  Convert #t to TRUE_TV and #f to FALSE_TV
"
    (stv (if b 1 0) 1))

(define-public (tv->bool tv)
"
  Convert TRUE_TV to #t, anything else to #f
"
    (equal? (stv 1 1) tv))

(define (atom->number A)
"
  Convert (NumberNode <number>) to number.
"
  (cog-number A))

;; Very handy and frequent rule precondition.
(define-public (gt-zero-confidence A)
"
  Return TrueTV iff A's confidence is greater than 0
"
  (bool->tv (> (cog-confidence A) 0)))

(define-public (gt-zero-confidence-eval A)
"
  Add the following evaluation in the current atomspace

  Evaluation
    GroundedPredicate \"scm: gt-zero-confidence-eval\"
    A
"
  (Evaluation
    (GroundedPredicate "scm: gt-zero-confidence-eval")
    A))

(define-public (absolutely-true A)
"
  Return TrueTV iff A's TV is TrueTV
"
  (bool->tv (tv->bool (cog-tv A))))

(define-public (absolutely-true-eval A)
"
  Add the following evaluation in the current atomspace

  Evaluation
    GroundedPredicate \"scm: absolutely-true\"
    A
"
  (Evaluation
    (GroundedPredicate "scm: absolutely-true")
    A))

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

(define (gen-variable prefix i)
"
  Generate variable (Variable prefix + \"-\" + (number->string i))
"
  (Variable (string-append prefix "-" (number->string i))))

(define (gen-variables prefix n)
"
  Generate a list of variables calling gen-variable with i=0,...,n-1
"
  (if (= n 0)
      ;; Base case
      '()
      ;; Recursive case
      (append (gen-variables prefix (- n 1))
              (list (gen-variable prefix (- n 1))))))

(define (cog-new-flattened-link link-type . args)
"
 Creates a new flattened link, for instance

   (cog-new-flattened-link 'AndLink (AndLink A B) C)

 will create the following

    (AndLink A B C)

 This is not recursive. So, for instance

   (cog-new-flattened-link 'AndLink (AndLink A (AndLink B)) C)

 will not produce

   (AndLink A B C)

 but will produce instead

   (AndLink A (AndLink B) C)

 Note that it will also remove duplicates, for instance

   (cog-new-flattened-link 'AndLink (AndLink A B C) C)

 will create the following

   (AndLink A B C)

 WARNING: TVs and other values attached to the atoms are ignored.
   The TV's and values are not copied to the new link, nor are they
   recomputed in any way.
"
  (define (flatten e r)
    (append r (if (and (cog-link? e) (equal? (cog-type e) link-type))
                  (cog-outgoing-set e)
                  (list e))))
  (let ((flat (delete-duplicates (fold flatten '() args))))
    (cog-new-link link-type flat)))

; ----------------------------------------------------------------------------
(define (simple-forward-step RB-NODE FOCUS-SET)
"
  Makes a single forward step using the rules in rulebase RB-NODE over the
  whole FOCUS-SET.
"
; NOTE: It is simple b/c it doesn't try to restrict inference over a
; certain source atoms.
; TODO: Move logic to ForwardChainer.
    (let* ((result (cog-fc RB-NODE (Set) #:focus-set (Set FOCUS-SET)))
           (result-list (cog-outgoing-set result)))
        ; Cleanup
        (cog-delete result)

        ; If there are multiple results for application of a rule, the
        ; result will have a ListLink of the results. Get the results out
        ; of the ListLinks helps in debugging and filtering-for-pln/sureal
        (receive (list-links other)
            (partition
                (lambda (x) (equal? 'ListLink (cog-type x))) result-list)

            (let ((partial-results (append-map cog-outgoing-set list-links)))
                ; Cleanup. NOTE: Cleanup is not done on `other` b/c, it might
                ; contain atoms which are part of r2l outputs, which if deleted
                ; recursively might affect the nlp pipline.
                (map cog-delete-recursive list-links)
                (delete-duplicates (append partial-results other))
            )
        )
    )
)

; ----------------------------------------------------------------------------
(define-public (simple-forward-chain RB-NODE FOCUS-SET STEPS)
"
  Applys the rules in rulebase RB-NODE over the whole FOCUS-SET, STEPS times.
  Before each recursive step occurs, the FOCUS-SET and outputs of current-step
  are merged and passed as the new FOCUS-SET.

  Returns a list containing both the FOCUS-SET and the inference results.
"
    ; TODO: Add an optional argument for filtering results b/n steps using.
    ; Create the next focus-set.
    (define (create-next-fs prev-fs chaining-result)
            (delete-duplicates (append chaining-result prev-fs)))

    (if (equal? 1 STEPS)
        (create-next-fs  FOCUS-SET (simple-forward-step RB-NODE FOCUS-SET))
        (simple-forward-chain RB-NODE
            (create-next-fs FOCUS-SET (simple-forward-step RB-NODE FOCUS-SET))
            (- STEPS 1))
    )
)

(define (export-ure-utils)
  (export
          cog-fc
          cog-bc
          ure-define-add-rule
          ure-add-rule
          ure-add-rule-by-name
          ure-add-rules-by-names
          ure-weighted-rules
          ure-rm-rule
          ure-rm-rule-by-name
          ure-rm-rules
          ure-rm-rules-by-names
          ure-set-num-parameter
          ure-set-fuzzy-bool-parameter
          ure-set-attention-allocation
          ure-set-maximum-iterations
          ure-set-complexity-penalty
          ure-set-jobs
          ure-set-fc-retry-exhausted-sources
          ure-set-bc-maximum-bit-size
          ure-set-bc-mm-complexity-penalty
          ure-set-bc-mm-compressiveness
          ure-define-rbs
          ure-get-forward-rule
          ure-logger-set-level!
          ure-logger-get-level
          ure-logger-set-filename!
          ure-logger-get-filename
          ure-logger-set-stdout!
          ure-logger-set-sync!
          ure-logger-set-timestamp!
          ure-logger-error-enabled?
          ure-logger-warn-enabled?
          ure-logger-info-enabled?
          ure-logger-debug-enabled?
          ure-logger-fine-enabled?
          ure-logger-error
          ure-logger-warn
          ure-logger-info
          ure-logger-debug
          ure-logger-fine
          ure-logger-flush
          bool->tv
          tv->bool
          atom->number
          gt-zero-confidence
          gt-zero-confidence-eval
          absolutely-true
          absolutely-true-eval
          meta-bind
          gen-variable
          gen-variables
          gen-rand-variable
          gen-rand-variables
          cog-new-flattened-link
  )
)
