;
; OpenCog test module exposing new Atom Types.
;
; The code below is all "boilerplate" -- keep the general form, and
; adjust the various names and paths to suit your needs. The various
; names and paths here *must* match those in the CMakefile.
;
(define-module (opencog test-demo-types))

(use-modules (opencog))
(use-modules (opencog test-chemodemo-config))

; Load the C library that calls the classserver to load the types.
; The library is `libtest-chem-types` and the library constructor
; is `chem_types_init`.
(load-extension
	(string-append opencog-ext-path-test-chemodemo "libtest-chem-types")
	"chem_types_init")

; The path below is the path of the types file.
(include-from-path "opencog/test-demo-types/chem_types.scm")
