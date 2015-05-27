;
; OpenCog Example module
;

; This must be the same name as in the C++ code
(define-module (opencog example))

; This must be the C++ library, and the initialization routien to call.
(load-extension "libexample" "opencog_example_init")
