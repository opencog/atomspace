;
; OpenCog Old Rule Engine module for backward compatibility
;
(define-module (opencog rule-engine))

; We need this to set the LTDL_LIBRARY_PATH
(use-modules (opencog))

(load-extension "libure" "opencog_ure_init")

(load-from-path "opencog/ure/ure-utils.scm")
(export-ure-utils)

(display "Deprecated module for backward compatibility only, use 'ure' instead\n")
