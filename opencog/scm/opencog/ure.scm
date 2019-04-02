;
; OpenCog Rule Engine module
;
(define-module (opencog ure))

; We need this to set the LTDL_LIBRARY_PATH
(use-modules (opencog))

(load-extension "libure" "opencog_ure_init")

(load-from-path "opencog/ure/ure-utils.scm")
(export-ure-utils)
