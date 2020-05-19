;
; OpenCog RandGen module
;
; Copyright (c) 2017 OpenCog Foundation
;

(define-module (opencog randgen))

(use-modules (opencog))
(use-modules (opencog as-config))
(load-extension (string-append opencog-ext-path-exec "librandgen") "opencog_randgen_init")

; Documentation for the functions implemented as C++ code
(set-procedure-property! cog-randgen-set-seed! 'documentation
"
 cog-randgen-set-seed! SEED

    Set the random seed to SEED

 Warning: this function is to control opencog's random generator.
          If you want to use scheme's native random generator
          use `*random-state*` instead.
")

(set-procedure-property! cog-randgen-randint 'documentation
"
 cog-randgen-randint N

    Return a random integer between 0 and N excluded.

 Warning: this function is to control opencog's random generator.
          If you want to use scheme's native random generator
          use `random` instead.
")

(set-procedure-property! cog-randgen-randfloat 'documentation
"
 cog-randgen-float

    Return a random float within [0, 1].

 Warning: this function is to control opencog's random generator.
          If you want to use scheme's native random generator
          use `random:uniform` instead.
")
