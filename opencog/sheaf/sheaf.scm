;
; Network inference module.
; Wraps up the assorted tools and scripts into one module.
;
(define-module (opencog sheaf))

(use-modules (opencog))

; The files are loaded in pipeline order.
; In general, the later files depend on definitions contained
; in the earlier files.
(load "sheaf/sections.scm")
(load "sheaf/mst-parser.scm")
(load "sheaf/vo-graph.scm")
(load "sheaf/make-section.scm")
