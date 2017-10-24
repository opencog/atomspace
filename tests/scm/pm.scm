;; File to reproduce a bug when operating on multiple atomspaces
;; created from scheme.

; Hack to get rule-engine to load in the unit-test environment.
(define path "/usr/local/lib/opencog:/usr/local/lib64/opencog")
(define path "./opencog/rule-engine:./opencog/guile")
(setenv "LTDL_LIBRARY_PATH"
   (if (getenv "LTDL_LIBRARY_PATH")
      (string-append (getenv "LTDL_LIBRARY_PATH") ":" path)
      path))

(use-modules (opencog))
(use-modules (opencog logger))
(use-modules (opencog query))
(use-modules (opencog rule-engine))

; Hack to re-load the logger module, again.
; Its been previously loaded, but the cog-logger-debug symbol
; is missing because it was loaded in a differen environment.
; guile environments are mis-handled in the unit tests...
(load "../../opencog/scm/opencog/logger.scm")

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
   (AndLink
      (Evaluation ;; <--- bug
         (GroundedPredicate "scm: my-precondition")
         (Variable "$X")
      )
      (Concept "I")
   )
   (Concept "O")
)
)

;; Run and-bit-prior rule base over bug-as and copy its results to
;; history-as.
(define (run-bug i)
  (cog-logger-debug "run-bug ~a" i)
  (cog-bind query)
  (gc)) ;; <--- precipitate the bug

(for-each run-bug (iota 100))
