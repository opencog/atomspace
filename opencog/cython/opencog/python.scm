;
; Python wrapper.  Allows python snippets to be executed from scheme.
;
(define-module (opencog python))

(use-modules (opencog))
(use-modules (opencog as-config))
(load-extension (string-append opencog-ext-path-python-scm "libPythonSCM")
	"opencog_python_init")

(export python-eval)

(set-procedure-property! python-eval 'documentation
"
 python-eval STRING
    Evaluate the STRING within the current python evaluator. The STRING
    argument can be any valid python code.

    Example: (python-eval \"print ('hello! ' + str(2+2))\")
")
