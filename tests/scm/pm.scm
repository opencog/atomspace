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

(define conditional-full-instantiation-implication-scope-meta-rule
  (let* ((V (Variable "$V"))
         (VariableT (Type "VariableNode"))
         (VariableListT (Type "VariableList"))
         (TypedVariableT (Type "TypedVariableLink"))
         (VardeclT (TypeChoice VariableT VariableListT TypedVariableT))
         (P (Variable "$P"))
         (Q (Variable "$Q"))

         ;; Meta rule variable declaration
         (meta-vardecl (VariableList
                         (TypedVariable V VardeclT)
                         P Q))
         ;; Meta rule main clause
         (implication (Quote
                        (ImplicationScope
                          (Unquote V)
                          (Unquote P)
                          (Unquote Q))))
         ;; Meta rule precondition
         (meta-precondition (Evaluation
                              (GroundedPredicate "scm: gt-zero-confidence")
                              implication))
         ;; Meta rule pattern
         (meta-pattern (And implication meta-precondition))

         ;; Produced rule variable declaration. P and Q will now be
         ;; content rather than variables.
         (produced-vardecl V)
         ;; Produced rule precondition. P must have a positive confidence
         (produced-precondition (Evaluation
                                  (GroundedPredicate "scm: gt-zero-confidence")
                                  P))
         ;; Produced rule pattern. Look for groundings of P that meet
         ;; the precondition.
         (produced-pattern (And P produced-precondition))
         ;; Produced rule rewrite. Apply formula to calculate the TV
         ;; over Q.
         (produced-rewrite (ExecutionOutput
                            (GroundedSchema "scm: conditional-full-instantiation-scope-formula")
                            (Unquote
                              (List
                                ;; Conclusion
                                Q
                                ;; Premises. Both P and the
                                ;; implication are required to
                                ;; calculate the TV over Q.
                                P
                                implication))))
         ;; Meta rule rewrite
         (meta-rewrite (Quote (Bind
                          (Unquote produced-vardecl)
                          (Unquote produced-pattern)
                          produced-rewrite ; the Unquote appears
                                           ; inside it, to avoid
                                           ; running the
                                           ; ExecutionOutput
                          ))))
    (Bind
      meta-vardecl
      meta-pattern
      meta-rewrite)))

(define (conditional-full-instantiation-scope-formula Qinst Pinst Impl)
  (cog-merge-hi-conf-tv! Qinst (stv 1 1)))

(define implication-scope
  (ImplicationScope (stv 0.0001 0.001)
    (VariableList     
      (Variable "$X")
      (Variable "$Y"))
    (And
      (Evaluation
        (Predicate "P")
        (Variable "$X"))
      (Evaluation
        (Predicate "P")
        (Variable "$Y")))
    (Predicate "Q")))

(define query (car (cog-outgoing-set (cog-bind conditional-full-instantiation-implication-scope-meta-rule))))

(cog-logger-debug "query = ~a" query)

;; Run and-bit-prior rule base over bug-as and copy its results to
;; history-as.
(define (run-bug i)
  (cog-logger-debug "run-bug ~a" i)
  (let* (;; Run and-bit-prior-rule over bug-as
         (results (cog-bind query)))  ;; <--- bug
    (gc) ;; <--- precipitate the bug
    (cog-logger-debug "failed yet?")))

(for-each run-bug (iota 100))

;; (format #t "These are ~A and ~A\n" post-init-as bug-as)
