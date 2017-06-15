;
; Correlation matrix analsysis module.
; Wraps up the assorted tools and scripts into one module.
;
(define-module (opencog analysis))

; The files are loaded in pipeline order.
; In general, the later files depend on definitions contained
; in the earlier files.
(load "analysis/atom-cache.scm")
(load "analysis/object-api.scm")
(load "analysis/support.scm")
(load "analysis/report-api.scm")
(load "analysis/fold-api.scm")
(load "analysis/bin-count.scm")
(load "analysis/cosine.scm")
(load "analysis/entropy.scm")
(load "analysis/compute-mi.scm")
(load "analysis/filter.scm")
(load "analysis/thresh-pca.scm")
(load "analysis/mst-parser.scm")
