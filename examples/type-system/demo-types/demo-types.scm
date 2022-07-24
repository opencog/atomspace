;
; OpenCog example module exposing new Atom Types.
;
; The code below is all "boilerplate" -- keep the general form, and
; adjust the various names and paths to suit your needs. The various
; names and paths here *must* match those in the CMakefile.
;
(define-module (opencog demo-types))

(use-modules (opencog))
(use-modules (opencog chemodemo-config))

; Load the C library that calls the classserver to load the types.
; The library is `libchem-demo-types` and the library constructor
; is `chem_types_init`.
(load-extension
	(string-append opencog-ext-path-chemodemo "libchem-demo-types")
	"chem_types_init")

; The path below is the path of the installed types file.
; Typically, it will be
;    /usr/local/share/guile/site/3.0/opencog/demo-types/chem_types.scm
(include-from-path "opencog/demo-types/chem_types.scm")
