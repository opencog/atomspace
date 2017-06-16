;
; Network inference module.
; Wraps up the assorted tools and scripts into one module.
;
(define-module (opencog sheaf))

; The files are loaded in pipeline order.
; In general, the later files depend on definitions contained
; in the earlier files.
(load "sheaf/mst-parser.scm")
(load "sheaf/make-section.scm")
