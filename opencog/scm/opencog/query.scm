;
; OpenCog Pattern matcher module
;

(define-module (opencog query))

; This is also loaded by (opencog exec) We need it here,
; else we get undefined symbols in libquery.
(load-extension "libexecution" "opencog_exec_init")

(load-extension "libquery" "opencog_query_init")
