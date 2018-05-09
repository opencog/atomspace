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

(set-procedure-property! cog-av 'documentation
"
 cog-av ATOM
    Return the attention value of ATOM.

    Example:
       ; Define a node
       guile> (define x
                 (cog-new-node 'ConceptNode \"abc\"
                    (cog-new-av 11 21 0)))
       guile> (cog-av x)
       (av 11 21 0)
       guile> (cog-av? (cog-av x))
       #t
")

(set-procedure-property! cog-set-av! 'documentation
"
 cog-set-av! ATOM AV
    Set the attention value of ATOM to AV.

    Example:
       ; Define a node
       guile> (define x (cog-new-node 'ConceptNode \"def\"))
       guile> (cog-av x)
       (av 0 0 0)
       guile> (cog-set-av! x (cog-new-av 44 55 1))
       (ConceptNode \"def\" (av 44 55 1))
       guile> (cog-av x)
       (av 44 55 1)
")

(set-procedure-property! cog-inc-vlti! 'documentation
"
 cog-inc-vlti! ATOM
    Increase the vlti of ATOM by 1.

    Example:
       ; Define a node
       guile> (define x
                 (cog-new-node 'ConceptNode \"abc\"
                    (cog-new-av 11 21 0)))
       guile> (cog-inc-vlti! x)
       (ConceptNode \"abc\" (av 11 21 1))
       guile> (cog-av x)
       (av 11 21 1)
       guile> (cog-inc-vlti! x)
       (ConceptNode \"abc\" (av 11 21 2))
       guile> (cog-av x)
       (av 11 21 2)
")

(set-procedure-property! cog-dec-vlti! 'documentation
"
 cog-dec-vlti! ATOM
    Decrease the vlti of ATOM by 1.

    Example:
       ; Define a node
       guile> (define x
                 (cog-new-node 'ConceptNode \"abc\"
                    (cog-new-av 11 21 1)))
       guile> (cog-dec-vlti! x)
       (ConceptNode \"abc\" (av 11 21 0))
       guile> (cog-av x)
       (av 11 21 0)
")

; -----------------------------------------------------------------------
(define-public (cog-set-sti! atom sti)
"
  Returns the atom after setting its sti to the given value.
"
    (let ((av-alist (cog-av->alist (cog-av atom))))
        (cog-set-av! atom
            (av sti (assoc-ref av-alist 'lti) (assoc-ref av-alist 'vlti)))
    )
)

(define-public (cog-set-lti! atom lti)
"
  Returns the atom after setting its lti to the given value.
"
    (let ((av-alist (cog-av->alist (cog-av atom))))
        (cog-set-av! atom
            (av (assoc-ref av-alist 'sti) lti (assoc-ref av-alist 'vlti)))
    )
)

(define-public (cog-set-vlti! atom vlti)
"
  Returns the atom after setting its vlti to the given value.
"
    (let ((av-alist (cog-av->alist (cog-av atom))))
        (cog-set-av! atom
            (av (assoc-ref av-alist 'sti) (assoc-ref av-alist 'lti) vlti))
    )
)

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
