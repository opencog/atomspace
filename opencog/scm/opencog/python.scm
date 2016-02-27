;
; Python wrapper.  Allows python snippets to be executed from scheme.
;

(define-module (opencog python))

; We need this to set the LTDL_LIBRARY_PATH
(use-modules (opencog))

(load-extension "libPythonSCM" "opencog_python_init")

(set-procedure-property! python-eval 'documentation
"
 python-eval STRING
    Evaluate the STRING within the current python evaluator. The STRING
    argument can be any valid python code.

    Example: (python-eval \"print ('hello! ' + str(2+2))\")
")
