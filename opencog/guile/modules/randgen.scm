;
; OpenCog RandGen module
;
; Copyright (c) 2017 OpenCog Foundation
;

(define-module (opencog randgen))

(use-modules (opencog))
(use-modules (opencog as-config))
(load-extension (string-append opencog-ext-path-exec "librandgen") "opencog_randgen_init")

(export
	cog-randgen-set-seed!
	cog-randgen-randint
	cog-randgen-randfloat
)

; Documentation for the functions implemented as C++ code
(set-procedure-property! cog-randgen-set-seed! 'documentation
"
 cog-randgen-set-seed! SEED

    Set the random seed of OpenCog's random generator to SEED.

    Warning: this function affects OpenCog's random generator only.
    If you want to set the seed of scheme's native random generator
    use `*random-state*` instead.

    OpenCog offers its own random generator singleton (defined in
    cogutil) to have a central point of control (random seed, etc)
    which can make it easier to achieve reproducibility. Note that
    it is neither guaranteed nor even desirable that all components
    use OpenCog's random generator. However for those that do, such
    a binding is provided.
")

(set-procedure-property! cog-randgen-randint 'documentation
"
 cog-randgen-randint N

    Return a random integer between 0 and N excluded using OpenCog's
    random generator.

    Warning: this function affects OpenCog's random generator only.
    If you want to select a random integer using scheme's native
    random generator, use `random` instead.

    OpenCog offers its own random generator singleton (defined in
    cogutil) to have a central point of control (random seed, etc)
    which can make it easier to achieve reproducibility. Note that
    it is neither guaranteed nor even desirable that all components
    use OpenCog's random generator. However for those that do, such
    a binding is provided.
")

(set-procedure-property! cog-randgen-randfloat 'documentation
"
 cog-randgen-float

    Return a random float within [0, 1].

    Warning: this function affects OpenCog's random generator only.
    If you want to select a random floating number using scheme's
    native random generator, use `random:uniform` instead.

    OpenCog offers its own random generator singleton (defined in
    cogutil) to have a central point of control (random seed, etc)
    which can make it easier to achieve reproducibility. Note that
    it is neither guaranteed nor even desirable that all components
    use OpenCog's random generator. However for those that do, such
    a binding is provided.
")
