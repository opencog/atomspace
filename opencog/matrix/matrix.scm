;
; Covariance matrix analysis module.
; Wraps up the assorted tools and scripts into one module.
;
(define-module (opencog matrix))

; ---------------------------------------------------------
; Common configuration
(use-modules (ice-9 threads))

; The guile-2.2 par-for-each implementation sucks, and live-locks
; for more than about 4-5 threads, and sometimes with less.
; The guile 2.9.4 par-for-each implemetation is mostly not insane;
; however, it appears to offer no speedup whatsoever over
; single-threaded operation ... for MI computations. (It does offer
; perfect 24x speedup on 24 cores for pattern matching... its unclear
; why it fails to speed up MI computations.)  Disable for now, since
; none of the matrix code benefits from this.
; XXX TODO diganose the root cause and report back to guile devels.
(define (maybe-par-for-each F L)
	; (par-for-each F L)
	(for-each F L)
)

; ---------------------------------------------------------
; The files are loaded in pipeline order.
; In general, the later files depend on definitions contained
; in the earlier files.
(load "matrix/eval-pair.scm")
(load "matrix/object-api.scm")
(load "matrix/dynamic.scm")
(load "matrix/support.scm")
(load "matrix/transpose.scm")
(load "matrix/report-api.scm")
(load "matrix/fold-api.scm")
(load "matrix/loop-api.scm")
(load "matrix/bin-count.scm")
(load "matrix/symmetric-mi.scm")
(load "matrix/cosine.scm")
(load "matrix/entropy.scm")
(load "matrix/compute-mi.scm")
(load "matrix/trans-batch.scm")
(load "matrix/filter.scm")
(load "matrix/similarity-api.scm")
(load "matrix/thresh-pca.scm")
