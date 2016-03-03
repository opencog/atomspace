;
; OpenCog Rule Engine module
;
(define-module (opencog rule-engine))

; We need this to set the LTDL_LIBRARY_PATH
(use-modules (opencog))

(load-extension "libruleengine" "opencog_ruleengine_init")

(load-from-path "rule-engine-utils.scm")
(export-rule-engine-utils)
