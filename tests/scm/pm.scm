;; File to reproduce a bug when operating on multiple atomspaces
;; created from scheme.

(use-modules (opencog logger))
(use-modules (opencog query))
(use-modules (opencog rule-engine))

;; Create a new atomspace to not by-pass the problem (due to
;; WORK_AROUND_GUILE_20_GC_BUG in SchemeSmobAS.cc)
(define post-init-as (cog-new-atomspace))

;; AtomSpace use to produce the bug. It crashes as soon as it gets
;; prematurely deleted
(define bug-as (cog-new-atomspace))
(cog-set-atomspace! bug-as) ;; <--- bug

;; Run and-bit-prior rule base over bug-as and copy its results to
;; history-as.
(define (run-bug i)
  (cog-logger-debug "run-bug ~a" i)
  (let* (;; Switch to bug-as
         ;; Load and-bit-prior rule base
         (dummy (load-from-path "tests/scm/pm-rule.scm"))
         ;; Define BC target and vardecl
         (target (Evaluation
                   (Predicate "URE:BC:preproof")
                   (List
                     (Variable "$A")
                     (Variable "$T"))))
         (vardecl (VariableList
                    (TypedVariable
                      (Variable "$A")
                      (Type "DontExecLink"))
                    (Variable "$T")))
         ;; Run and-bit-prior-rule over bug-as
         (results (cog-bind and-bit-prior-rule)))  ;; <--- bug
    (gc) ;; <--- precipitate the bug
    (cog-logger-debug "failed yet?")))

(for-each run-bug (iota 100))
