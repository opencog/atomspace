;
; OpenCog Example module
;

; The name of the module must be the same as the name used in the
; `define_scheme_primitive` call in the C++ code.
(define-module (opencog example))

; The first argument is the C++ library, and the second is the
; initialization routine to call. If the library is installed into
; the default shared library search path (i.e. /usr/local/lib/opencog),
; then the sring-append is not needed. However, we are not installing
; the example library, and so the explicit path is needed.
(load-extension
	(string-append (getcwd) "/examples/c++-guile/libexample")
	"opencog_example_init")

; If the module was installed, then this would be enough:
; (load-extension "libexample" "opencog_example_init")
