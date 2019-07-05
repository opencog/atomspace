;
; OpenCog Old Rule Engine module for backward compatibility
;
(define-module (opencog rule-engine))

(use-modules (opencog))
(use-modules (opencog as-config))
(load-extension (string-append opencog-ext-path-exec "libure") "opencog_ure_init")

(load-from-path "opencog/ure/ure-utils.scm")
(export-ure-utils)

(display "Deprecated module for backward compatibility only, use 'ure' instead\n")
