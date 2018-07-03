;
; Covariance matrix analysis module.
; Wraps up the assorted tools and scripts into one module.
;
(define-module (opencog matrix))

; The files are loaded in pipeline order.
; In general, the later files depend on definitions contained
; in the earlier files.
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
