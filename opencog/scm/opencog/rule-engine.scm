;
; OpenCog Rule Engine module
;

(define-module (opencog rule-engine))

(load-extension "libruleengine" "opencog_ruleengine_init")

(load-from-path "rule-engine-utils.scm")
(export-rule-engine-utils)
