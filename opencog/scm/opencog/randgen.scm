;
; OpenCog RandGen module
;
; Copyright (c) 2017 OpenCog Foundation
;

(define-module (opencog randgen))

(use-modules (opencog))
(use-modules (opencog as-config))
(load-extension (string-append opencog-ext-path-exec "librandgen") "opencog_randgen_init")

(display "\n")
(display "====> Attention!\n")
(display "====> Deprecated! Please port your code to use the native\n")
(display "====> scheme random number functions. See, for example,\n")
(display "====> https://www.gnu.org/software/guile/manual/html_node/Random.html\n")
(display "\n")

; Documentation for the functions implemented as C++ code
(set-procedure-property! cog-randgen-set-seed! 'documentation
"
 cog-randgen-set-seed! SEED
    Deprecated! Please use scheme's native `*random-state*` instead!

    Set the random seed to SEED
")

(set-procedure-property! cog-randgen-randint 'documentation
"
 cog-randgen-randint N
    Deprecated! Please use scheme's native `random` instead!

    Return a random integer between 0 and N excluded.
")

(set-procedure-property! cog-randgen-randfloat 'documentation
"
 cog-randgen-float
    Deprecated! Please use scheme's native `random:uniform` instead!

    Return a random float within [0, 1].
")
