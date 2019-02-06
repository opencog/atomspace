;; File to reproduce a bug when operating on multiple atomspaces
;; created from scheme.

(use-modules (opencog))
(use-modules (opencog exec))
(use-modules (opencog logger))

;; Create a new atomspace to not by-pass the problem (due to
;; WORK_AROUND_GUILE_20_GC_BUG in SchemeSmobAS.cc)
(define post-init-as (cog-new-atomspace))

;; AtomSpace use to produce the bug. It crashes as soon as it gets
;; prematurely deleted
(define bug-as (cog-new-atomspace))
(cog-set-atomspace! bug-as) ;; <--- bug

(define (my-precondition X)
  (stv 1 1))

(define query
(Bind
   (TypedVariable (Variable "$X") (TypeNode "ConceptNode"))
   (AndLink
      (Evaluation ;; <--- bug
         (GroundedPredicate "scm: my-precondition")
         (Variable "$X")
      )
      (Present (Variable "$X"))
      (Concept "I")
   )
   (Concept "O")
)
)

;; Run and-bit-prior rule base over bug-as
(define (run-bug i)
  (cog-logger-debug "run-bug ~a" i)
  (cog-execute! query)
  (gc)) ;; <--- precipitate the bug

(for-each run-bug (iota 100))
