;
; tv.scm
;
; Useful utilities for working with truth values.
;
; Utilities provided:
; -- cog-merge-tv! -- Merge truth values on atom
; -- cog-merge-hi-conf-tv! -- Different merge style
;
; Copyright (c) 2014 Cosmo Harrigan
;
; ===================================================================
; Simple wrappers for TruthValues

(define-public (cog-new-stv MEAN CONFIDENCE)
"
 cog-new-stv MEAN CONFIDENCE
    Create a SimpleTruthValue with the given MEAN and CONFIDENCE.
    Equivalent to (cog-new-value 'SimpleTruthValue MEAN CONFIDENCE)

    Unlike Atoms, Values are ephemeral: they are automatically
    garbage-collected when no longer needed.

    Throws error if MEAN and CONFIDENCE are not numeric values.
    Example:
        ; Create a new simple truth value:
        guile> (cog-new-stv 0.7 0.9)
"
	(cog-new-value 'SimpleTruthValue MEAN CONFIDENCE)
)

(define-public (cog-new-ctv MEAN CONFIDENCE COUNT)
"
 cog-new-ctv MEAN CONFIDENCE COUNT
    Create a CountTruthValue with the given MEAN, CONFIDENCE and COUNT.
    Equivalent to
    (cog-new-value 'CountTruthValue MEAN CONFIDENCE COUNT)

    Unlike Atoms, Values are ephemeral: they are automatically
    garbage-collected when no longer needed.

    Throws error if MEAN, CONFIDENCE and COUNT are not numeric values.
    Example:
        ; Create a new count truth value:
        guile> (cog-new-ctv 0.7 0.9 44.0)
"
	(cog-new-value 'CountTruthValue MEAN CONFIDENCE COUNT)
)

(define-public (cog-new-itv LOWER UPPER CONFIDENCE)
"
 cog-new-itv LOWER UPPER CONFIDENCE
    Create an IndefiniteTruthValue with the given LOWER, UPPER and
    CONFIDENCE.  Equivalent to
    (cog-new-value 'IndefiniteTruthValue LOWER UPPER CONFIDENCE)

    Unlike Atoms, Values are ephemeral: they are automatically
    garbage-collected when no longer needed.

    Throws error if LOWER, UPPER and CONFIDENCE are not numeric values.
    Example:
        ; Create a new indefinite truth value:
        guile> (cog-new-itv 0.7 0.9 0.6)
"
	(cog-new-value 'IndefiniteTruthValue LOWER UPPER CONFIDENCE)
)

(define-public (cog-new-etv POSITIVE-COUNT TOTAL-COUNT)
"
 cog-new-etv POSITIVE-COUNT TOTAL-COUNT
    Create an EvidenceCountTruthValue with the given POSITIVE-COUNT
    and TOTAL-COUNT. Equivalent to
    (cog-new-value 'EvidenceCountTruthValue POSITIVE-COUNT TOTAL-COUNT)

    Unlike Atoms, Values are ephemeral: they are automatically
    garbage-collected when no longer needed.

    The total count is optional in the sense that any value below the
    positive count will be considered undefined.

    Throws error if positive-count and total-count are not numeric
    values.
    Example:
        ; Create a new simple truth value:
        guile> (cog-new-etv 100 150)
"
	(cog-new-value 'EvidenceCountTruthValue POSITIVE-COUNT TOTAL-COUNT)
)

(define-public (cog-new-ftv MEAN CONFIDENCE)
"
 cog-new-ftv MEAN CONFIDENCE
    Create a FuzzyTruthValue with the given MEAN and CONFIDENCE.
    Equivalent to (cog-new-value 'FuzzyTruthValue MEAN CONFIENCE)

    Unlike Atoms, Values are ephemeral: they are automatically
    garbage-collected when no longer needed.

    Throws error if MEAN or CONFIDENCE are not numeric values.
    Example:
        ; Create a new fuzzy truth value:
        guile> (cog-new-ftv 0.7 0.9)
"
	(cog-new-value 'FuzzyTruthValue MEAN CONFIDENCE)
)

(define-public (cog-new-ptv MEAN CONFIDENCE COUNT)
"
 cog-new-ptv MEAN CONFIENCE COUNT
    Create a ProbabilisticTruthValue with the given MEAN, CONFIDENCE
    and COUNT.  Equivalent to
    (cog-new-value 'ProbabilisticTruthValue MEAN CONFIENCE COUNT)

    Unlike Atoms, Values are ephemeral: they are automatically
    garbage-collected when no longer needed.

    Throws errors if MEAN, CONFIDENCE and COUNT are not numeric values.
    Example:
        ; Create a new probabilistic truth value:
        guile> (cog-new-ptv 0.7 0.9 44.0)
"
	(cog-new-value 'ProbabilisticTruthValue MEAN CONFIDENCE COUNT)
)

(define-public (stv mean conf) (cog-new-stv mean conf))
(define-public (ctv mean conf count) (cog-new-ctv mean conf count))
(define-public (itv lower upper conf) (cog-new-itv lower upper conf))
(define-public (etv pos-count total-count) (cog-new-etv pos-count total-count))
(define-public (ftv mean conf) (cog-new-ftv mean conf))
(define-public (ptv mean conf count) (cog-new-ptv mean conf count))

; ===================================================================

(define-public (cog-tv? EXP)
"
 cog-tv? EXP
    Return #t if EXP is a TruthValue, else return #f
    Equivalent to (cog-subtype? 'TruthValue (cog-type EXP))

    Example:
       ; Define a simple truth value
       guile> (define x (cog-new-stv 0.7 0.9))
       guile> (define y (+ 2 2))
       guile> (cog-tv? x)
       #t
       guile> (cog-tv? y)
       #f
"
	(cog-subtype? 'TruthValue (cog-type EXP))
)

(define-public (cog-ctv? EXP)
"
 cog-ctv? EXP
    Return #t if EXP is a CountTruthValue, else return #f.
    Equivalent to (equal? 'CountTruthValue (cog-type EXP))
"
	(equal? 'CountTruthValue (cog-type EXP))
)

; ===================================================================

(define-public (tv-positive-count TV)
"
  Return the floating-point positive count of a EvidenceCountTruthValue.
"
	(if (equal? 'EvidenceCountTruthValue (cog-type TV))
		(car (cog-value->list TV))
		0)
)

; -----------------------------------------------------------------------
(define-public (cog-merge-tv! ATOM TV)
" cog-merge-tv! -- merge truth values on atom"
	(cog-set-tv! ATOM (cog-tv-merge (cog-tv ATOM) TV))
)

; -----------------------------------------------------------------------
(define-public (cog-merge-hi-conf-tv! ATOM TV)
" cog-merge-hi-conf-tv! -- merge truth values on atom"
	(cog-set-tv! ATOM (cog-tv-merge-hi-conf (cog-tv ATOM) TV))
)

; -----------------------------------------------------------------------
