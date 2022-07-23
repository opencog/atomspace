;
; OpenCog example module exposing new Atom Types.
;
; The code below is all "boilerplate" -- keep the general form, and
; adjust the various names and paths to suit your needs.
;
(define-module (opencog demo-types))

(use-modules (opencog))
(use-modules (opencog chemodemo-config))

; Load the C library that calls the classserver to load the types.
(load-extension
	(string-append opencog-ext-path-chemodemo "libchem-types")
	"chem_types_init")

(load-from-path "opencog/demo-types/chem_types.scm")
