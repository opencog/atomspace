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

(define query
(BindLink
   (VariableList
      (VariableNode "$X")
      (VariableNode "$Y")
   )
   (AndLink
      (EvaluationLink
         (GroundedPredicateNode "scm: gt-zero-confidence")
         (AndLink
            (EvaluationLink
               (PredicateNode "P")
               (VariableNode "$Y")
            )
            (EvaluationLink
               (PredicateNode "P")
               (VariableNode "$X")
            )
         )
      )
      (AndLink
         (EvaluationLink
            (PredicateNode "P")
            (VariableNode "$Y")
         )
         (EvaluationLink
            (PredicateNode "P")
            (VariableNode "$X")
         )
      )
   )
   (ExecutionOutputLink
      (GroundedSchemaNode "scm: conditional-full-instantiation-scope-formula")
      (ListLink
         (PredicateNode "Q")
         (AndLink
            (EvaluationLink
               (PredicateNode "P")
               (VariableNode "$Y")
            )
            (EvaluationLink
               (PredicateNode "P")
               (VariableNode "$X")
            )
         )
         (ImplicationScopeLink (stv 0.0001 0.001)
            (VariableList
               (VariableNode "$X")
               (VariableNode "$Y")
            )
            (AndLink
               (EvaluationLink
                  (PredicateNode "P")
                  (VariableNode "$Y")
               )
               (EvaluationLink
                  (PredicateNode "P")
                  (VariableNode "$X")
               )
            )
            (PredicateNode "Q")
         )
      )
   )
)
)

;; Run and-bit-prior rule base over bug-as and copy its results to
;; history-as.
(define (run-bug i)
  (cog-logger-debug "run-bug ~a" i)
  (cog-bind query)
  (gc) ;; <--- precipitate the bug
  (cog-logger-debug "failed yet?"))

(for-each run-bug (iota 100))
