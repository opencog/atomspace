;
; tv.scm
;
; Useful utilities for working with truth values.
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

(define-public (stv mean conf) (cog-new-stv mean conf))
(define-public (ctv mean conf count) (cog-new-ctv mean conf count))

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

(define-public (cog-tv-mean TV)
"
 cog-tv-mean TV
    Return the `mean` of the TruthValue TV. This is a single
    floating point-number.

    See also: cog-mean
"
	(cog-value-ref TV 0)
)

(define-public (cog-tv-confidence TV)
"
 cog-tv-confidence TV
    Return the `confidence` of the TruthValue TV. This is a single
    floating point-number.

    See also: cog-confidence
"
	(cog-value-ref TV 1)
)

(define-public (cog-tv-count TV)
"
 cog-tv-count TV
    Return the `count` of the TruthValue TV. This is a single
    floating point-number.

    See also: cog-count
"
	(define DEFAULT_K 800.0)
	(if (cog-ctv? TV)
		(cog-value-ref TV 2)
		(let ((conf (cog-tv-confidence TV)))
			(/ (* DEFAULT_K conf) (- 1.00000001 conf))))
)

; ===================================================================

(define-public (cog-set-tv! ATOM TV)
"
 cog-set-tv! ATOM TV
    Set the truth-value of ATOM to TV.

    Example:
       ; Define a node
       guile> (define x (Concept \"def\"))
       guile> (cog-tv x)
       (stv 1 0)
       guile> (cog-set-tv! x (SimpleTruthValue 0.9 0.8))
       (ConceptNode \"def\" (stv 0.9 0.8))
       guile> (cog-tv x)
       (stv 0.9 0.8)
"
	(define tvkey (Predicate "*-TruthValueKey-*"))
	(cog-set-value! ATOM tvkey TV)
)

; ===================================================================

(define-public (cog-inc-count! ATOM CNT)
"
  cog-inc-count! ATOM CNT -- Increment count truth value on ATOM by CNT.

  Atomically increment the count on a CountTruthValue by CNT. The mean
  and confidence values are left untouched.  CNT may be any floating
  point number (positive or negative).

  If the current truth value on the ATOM is not a CountTruthValue,
  then the truth value is replaced by a CountTruthValue, with the
  count set to CNT.

  The increment is atomic; that is, it is safe against racing threads.

  Example usage:
     (cog-inc-count! (Concept \"Answer\") 42.0)

  See also:
      cog-count -- Fetch the current count.
      cog-inc-value! -- Increment an arbitrary FloatValue.
      cog-update-value! -- A generic atomic read-modify-write.
"
	(define tvkey (Predicate "*-TruthValueKey-*"))
	(catch #t
		(lambda () (cog-inc-value! ATOM tvkey CNT 2))
		(lambda (key . args) (cog-set-value! ATOM tvkey (ctv 1 0 CNT))))
)

; ===================================================================
