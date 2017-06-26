;
; OpenCog RandGen module
;
; Copyright (c) 2017 OpenCog Foundation
;

(define-module (opencog randgen))

; We need this to set the LTDL_LIBRARY_PATH
(use-modules (opencog))

(load-extension "librandgen" "opencog_randgen_init")

; Documentation for the functions implemented as C++ code
(set-procedure-property! cog-randgen-set-seed! 'documentation
"
 cog-randgen-set-seed! SEED
    Set the random seed to SEED
")

(set-procedure-property! cog-randgen-randint 'documentation
"
 cog-randgen-randint N
    Return a random integer between 0 and N excluded.
")
