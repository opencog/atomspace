;;
;; rule-engine-utils.scm
;;
;;;; Commentary:
;;
;; Handy utilities for working with the rule-engine. In particular to
;; configure a rule-based system (rbs).
;;
;; Utilities include:
;; -- ure-add-rule -- Associate a rule to a rbs with a certain TV
;; -- ure-add-rules -- Associate  a list of rule-alias and TV pairs to a rbs
;; -- ure-set-num-parameter -- Set a numeric parameter of an rbs
;; -- ure-set-fuzzy-bool-parameter -- Set a fuzzy boolean parameter of an rbs
;; -- ure-set-attention-allocation -- Set the URE:attention-allocation parameter
;; -- ure-set-maximum-iterations -- Set the URE:maximum-iterations parameter
;; -- ure-set-fc-retry-sources -- Set the URE:FC:retry-sources parameter
;; -- ure-set-bc-complexity-penalty -- Set the URE:BC:complexity-penalty parameter
;; -- ure-set-bc-maximum-bit-size -- Set the URE:BC:maximum-bit-size
;; -- ure-set-bc-mm-complexity-penalty -- Set the URE:BC:MM:complexity-penalty
;; -- ure-set-bc-mm-compressiveness -- Set the URE:BC:MM:compressiveness
;; -- ure-define-rbs -- Create a rbs that runs for a particular number of
;;                      iterations.
;; -- ure-get-forward-rule -- Return the forward form of a rule
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
;;
;; If you add more utilities don't forget to add them in the
;; export-rule-engine-utils function.
;;
;;;; Code:
;; Copyright (c) 2015, OpenCog Foundation
;;

(use-modules (opencog))
(use-modules (opencog exec))
(use-modules (opencog logger))
(use-modules (srfi srfi-1))

(define* (cog-fc rbs source #:key (vardecl (List)) (focus-set (Set)))
"
  Forward Chainer call.

  Usage: (cog-fc rbs source #:vardecl vd #:focus-set fs)

  rbs: ConceptNode representing a rulebase.

  source: Source from where to start forward chaining. If a SetLink
          then multiple sources are considered.

  vd: optional variable declaration of the source (in case it has
      variables)

  fs: optional focus-set, a SetLink with all atoms to consider for
      forward chaining
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

  vardecl: [optional] Variable declaration of the target (in case it
           has variables)

  trace-as: [optional] AtomSpace to record the back-inference traces.

  control-as: [optional] AtomSpace storing inference control rules.

  focus-set: [optional] focus-set, a SetLink with all atoms to
             consider for forward chaining (NOT IMPLEMENTED).
"
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

(define (ure-set-num-parameter rbs name value)
"
  Set numerical parameters. Given an rbs, a parameter name and its
  value, create

  ExecutionLink
     SchemaNode name
     rbs
     NumberNode value

  It will also delete the any

  ExecutionLink
     SchemaNode name
     rbs
     *

  to be sure there is ever only one value associated to that
  parameter. The value is automatically converted into string.
"
  (define (param-hypergraph atom)
    (ExecutionLink
       (SchemaNode name)
       rbs
       atom)
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
  (param-hypergraph (NumberNode value))
)

(define (ure-set-fuzzy-bool-parameter rbs name value)
"
  Set (fuzzy) bool parameters. Given an RBS, a parameter name and its
  value, create (or overwrite)

  EvaluationLink (stv value 1)
     PredicateNode name
     rbs

  If the provided value is a boolean, then it is automatically
  converted into tv.
"
  (let* ((tv (if (number? value) (stv value 1) (bool->tv value))))
    (EvaluationLink tv
      (PredicateNode name)
      rbs)))

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
  (ure-set-num-parameter rbs "URE:attention-allocation" value))

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

(define (ure-set-fc-retry-sources rbs value)
"
  Set the URE:FC:retry-sources parameter of a given RBS

  EvaluationLink (stv value 1)
    PredicateNode \"URE:FC:retry-sources\"
    rbs

  If the provided value is a boolean, then it is automatically
  converted into tv.
"
  (ure-set-fuzzy-bool-parameter rbs "URE:FC:retry-sources" value))

(define (ure-set-bc-complexity-penalty rbs value)
"
  Set the URE:BC:complexity-penalty parameter of a given RBS

  ExecutionLink
    SchemaNode \"URE:BC:complexity-penalty\"
    rbs
    NumberNode value

  Delete any previous one if exists.
"
  (ure-set-num-parameter rbs "URE:BC:complexity-penalty" value))

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

(define-public (ure-get-forward-rule rule)
"
  Given a rule return the forward form
"
  (let ((rule-type (cog-type rule)))
    (if (eq? rule-type 'ListLink) (gar rule) rule))
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
  (string->number (cog-name A)))

;; Very handy and frequent rule precondition.
(define-public (gt-zero-confidence A)
"
  Return TrueTV iff A's confidence is greater than 0
"
  (bool->tv (> (cog-stv-confidence A) 0)))

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

;; TODO: use random-variable instead
(define (gen-rand-variable prefix base length)
"
  gen-rand-variable prefix base length

  Generate a random variable (Variable prefix + \"-\" + RAND). where
  RAND is a random sequence of 'length' digits in base 'base'.
"
  (let* ((rand-char (lambda (i) (number->string (random base) base)))
         (rand-chars (map rand-char (iota length)))
         (rand-str (apply string-append rand-chars))
         (rand-name (string-append prefix "-" rand-str)))
    (Variable rand-name)))

(define (gen-rand-variables prefix base length n)
"
  Generate a list of random variables calling gen-rand-variable n times.
"
  (map (lambda (x) (gen-rand-variable prefix base length)) (iota n)))

(define (export-rule-engine-utils)
  (export
          cog-fc
          cog-bc
          ure-define-add-rule
          ure-add-rule
          ure-add-rules
          ure-set-num-parameter
          ure-set-fuzzy-bool-parameter
          ure-set-attention-allocation
          ure-set-maximum-iterations
          ure-set-fc-retry-sources
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
  )
)
