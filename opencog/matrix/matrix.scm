;
; Covariance matrix analysis module.
; Wraps up the assorted tools and scripts into one module.
;
(define-module (opencog matrix))
(use-modules (opencog))

; ---------------------------------------------------------
; Common configuration
(use-modules (ice-9 threads))

; The guile-2.2 par-for-each implementation sucks, and live-locks
; for more than about 4-5 threads, and sometimes with less.
; The guile-2.9.4 par-for-each implementation is mostly not insane;
; however, it appears to offer no speedup whatsoever over
; single-threaded operation ... for MI computations. (It does offer
; perfect 24x speedup on 24 cores for pattern matching... its unclear
; why it fails to speed up MI computations.)  Disable for now, since
; none of the matrix code benefits from this.
; XXX TODO diagnose the root cause and report back to guile devels.
;
; As of guile-3.0.1 in Debian stable, manually running similarity calcs
; in distinct threads offers no speedup over single-threaded perf. That
; is, even if we don't use par-for-each and do it manually, there's no
; speedup. Why? The C++ atomspace is parallelizable. Maybe there's some
; serialization in enter/exit guile?
;
(define (maybe-par-for-each F L)
	; (par-for-each F L)
	(for-each F L)
)

; ---------------------------------------------------------
; The files are loaded in pipeline order.
; In general, the later files depend on definitions contained
; in the earlier files.
(include-from-path "opencog/matrix/progress.scm")
(include-from-path "opencog/matrix/eval-pairs.scm")
(include-from-path "opencog/matrix/object-api.scm")
(include-from-path "opencog/matrix/dynamic.scm")
(include-from-path "opencog/matrix/store.scm")
(include-from-path "opencog/matrix/frequency.scm")
(include-from-path "opencog/matrix/support.scm")
(include-from-path "opencog/matrix/transpose.scm")
(include-from-path "opencog/matrix/report-api.scm")
(include-from-path "opencog/matrix/fold-api.scm")
(include-from-path "opencog/matrix/loop-api.scm")
(include-from-path "opencog/matrix/bin-count.scm")
(include-from-path "opencog/matrix/symmetric-mi.scm")
(include-from-path "opencog/matrix/cosine.scm")
(include-from-path "opencog/matrix/group-similarity.scm")
(include-from-path "opencog/matrix/entropy.scm")
(include-from-path "opencog/matrix/compute-mi.scm")
(include-from-path "opencog/matrix/trans-batch.scm")
(include-from-path "opencog/matrix/filter.scm")
(include-from-path "opencog/matrix/trim.scm")
(include-from-path "opencog/matrix/similarity-api.scm")
(include-from-path "opencog/matrix/direct-sum.scm")
(include-from-path "opencog/matrix/thresh-pca.scm")
