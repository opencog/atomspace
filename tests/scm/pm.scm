;; File to reproduce a bug when operating on multiple atomspaces
;; created from scheme.

; Hack to get rule-engine to load in the unit-test environment.
; I don't understand why this is needed here, it doesn't seem to
; be needed in any of the other unit tests.
(define libpath "/usr/local/lib/opencog:/usr/local/lib64/opencog")
(define libpath "./opencog/rule-engine:./opencog/guile:../../opencog/rule-engine:../../opencog/guile:../../../opencog/rule-engine:../../../opencog/guile")
; (define libpath "./opencog/rule-engine:./opencog/guile")
(setenv "LTDL_LIBRARY_PATH"
   (if (getenv "LTDL_LIBRARY_PATH")
      (string-append (getenv "LTDL_LIBRARY_PATH") ":" libpath)
      libpath))

(reload-module (resolve-module (list (string->symbol "opencog"))))
(reload-module (resolve-module (list (string->symbol "opencog") (string->symbol "logger"))))
(use-modules (opencog))
(use-modules (opencog exec))
(use-modules (opencog logger))
(use-modules (opencog rule-engine))
(sleep 2)
; (format #t "duuude mod is ~A\n" (current-module))

; (reload-module ((opencog logger)))
; (reload-module (resolve-module ((opencog logger))))
; (format #t "duuude now amod is ~A\n" (current-module))

; Hack to re-load the logger module, again.
; Its been previously loaded, but the `cog-logger-debug` symbol
; is missing because it was loaded in a different environment.
; guile environments are mis-handled in the unit tests...
; (load "../../opencog/scm/opencog/logger.scm")
; (display %load-path) (newline)
(add-to-load-path "../opencog/scm/opencog/")
(add-to-load-path "../../opencog/scm/opencog/")
; (save-module-excursion (lambda () (load-from-path "exec.scm")))
; (save-module-excursion (lambda () (load-from-path "logger.scm")))

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
