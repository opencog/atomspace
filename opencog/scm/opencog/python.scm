;
; Python wrapper.  Allows python snippets to be executed from scheme.
;

(define-module (opencog python))

; We need this to set the LTDL_LIBRARY_PATH
(use-modules (opencog))

(load-extension "libPythonSCM" "opencog_python_init")
