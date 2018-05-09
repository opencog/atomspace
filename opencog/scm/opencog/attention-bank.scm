;
; OpenCog AttentionBank module
; Copyright (C) 2018 Linas Vepstas <linasvepstas@gmail.com>
;

(define-module (opencog attention-bank))

(load-extension "libattentionbank" "opencog_attention_init")

; This avoids complaints, when the docs are set, below.
(export cog-update-af cog-af-size cog-set-af-size! cog-stimulate)

(use-modules (opencog))

;; -----------------------------------------------------
;;

(define* (cog-af #:optional (n -1))
"
 cog-af n
    Return the top n atoms in the AttentionalFocus or
    return all atoms in the AF if n is unspecified or is larger
    than the AF size.

    Example:
    guile> (cog-af)
    (ConceptNode \"ArtificialIntelligence\" (av 15752 0 0))
    (ConceptNode \"Databases\" (av 15752 0 0))

    guile> (cog-af 1)
    (ConceptNode \"ArtificialIntelligence\" (av 15752 0 0))
"
	(map gar
		(cog-incoming-by-type (cog-update-af n) 'MemberLink))
)

(export cog-af)

; -----------------------------------------------------------------------

(define-public (cog-af-length)
" cog-af-length -- Length of the list of atoms in the attentional focus."
	(length (cog-af))
)

;; -----------------------------------------------------
;;

(set-procedure-property! cog-af-size 'documentation
"
 cog-af-size
    Return the AttentionalFocus size of the AtomSpace (which is
    an integer value).

    Example:

    guile> (cog-af-size)
    100
")

(set-procedure-property! cog-set-af-size! 'documentation
"
 cog-set-af-size! AF Size
    Set the AttentionalFocus Size of the AtomSpace (which is an
    integer value). Returns the new AttentionalFocus size
    (which is an integer value).

    Example:
    guile> (cog-set-af-size! 200)
    200
")


; --------------------------------------------------------------------
