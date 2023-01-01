;
; Network inference module.
; Wraps up the assorted tools and scripts into one module.
;
(define-module (opencog sheaf))

(use-modules (opencog))
(use-modules (ice-9 format))

; The files are loaded in pipeline order.
; In general, the later files depend on definitions contained
; in the earlier files.
(include-from-path "opencog/sheaf/sections.scm")
(include-from-path "opencog/sheaf/vo-graph.scm")
(include-from-path "opencog/sheaf/linear-parser.scm")
(include-from-path "opencog/sheaf/mst-parser.scm")
(include-from-path "opencog/sheaf/mpg-parser.scm")
(include-from-path "opencog/sheaf/make-section.scm")
