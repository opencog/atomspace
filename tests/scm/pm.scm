;; File to reproduce a bug when operating on multiple atomspaces
;; created from scheme.

; Hack to get logger to load in the unit-test environment.
; I don't understand why this is needed here, it doesn't seem to
; be needed in any of the other unit tests.
; Its got some bizarro interaction with the rule-engine: if we
; remove the rule-engine from the path, then liblogger.scm cannot
; be found! That is very confusing to me ...
(define libpath "/usr/local/lib/opencog:/usr/local/lib64/opencog")
(define libpath "./opencog/rule-engine:./opencog/guile:../../opencog/rule-engine:../../opencog/guile")
(setenv "LTDL_LIBRARY_PATH"
   (if (getenv "LTDL_LIBRARY_PATH")
      (string-append (getenv "LTDL_LIBRARY_PATH") ":" libpath)
      libpath))

(use-modules (opencog logger))

; Hack to reload the logger module. I don't understand why this is
; needed, but its needed. It might have something to do with unresolved
; symbols in some of the libraries that liblgger.so depends on?
; (reload-module (resolve-module (list (string->symbol "opencog"))))
(reload-module (resolve-module (list (string->symbol "opencog") (string->symbol "logger"))))

(use-modules (opencog exec))
(use-modules (opencog rule-engine))

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

;; Run and-bit-prior rule base over bug-as and copy its results to
;; history-as.
(define (run-bug i)
  (cog-logger-debug "run-bug ~a" i)
  (cog-execute! query)
  (gc)) ;; <--- precipitate the bug

(for-each run-bug (iota 100))
