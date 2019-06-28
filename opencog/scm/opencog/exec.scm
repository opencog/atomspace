;
; OpenCog Execution module
;

(define-module (opencog exec))

(use-modules (opencog as-config))
(load-extension (string-append opencog-ext-path-exec "libexec") "opencog_exec_init")

(export cog-evaluate! cog-execute!)
