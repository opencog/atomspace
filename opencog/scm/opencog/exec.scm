;
; OpenCog Execution module
;

(define-module (opencog exec))

; We need this to set the LTDL_LIBRARY_PATH
(use-modules (opencog))

(load-extension "libexecution" "opencog_exec_init")
