;
; OpenCog Logger module
;

(define-module (opencog logger))

(load-extension "libsmob" "opencog_logger_init")

(load-from-path "logger-utils.scm")
(export-logger-utils)

