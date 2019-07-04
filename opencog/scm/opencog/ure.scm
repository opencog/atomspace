;
; OpenCog Unified Rule Engine module
;
(define-module (opencog ure))

(use-modules (opencog as-config))
(load-extension (string-append opencog-ext-path-ure "libure") "opencog_ure_init")

(load-from-path "opencog/ure/ure-utils.scm")
(export-ure-utils)
