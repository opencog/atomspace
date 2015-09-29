;
; OpenCog Pattern matcher module
;

(define-module (opencog dist-gearman))

; This is also loaded by (opencog exec) We need it here,
; else we get undefined symbols in libquery.
;(load-extension "libexecution" "opencog_exec_init")

(load-extension "libdist-gearman" "opencog_dist_init")
