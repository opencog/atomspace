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

(define-public (stv mean conf) (cog-new-stv mean conf))

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
	(if EXP (cog-subtype? 'TruthValue (cog-type EXP)) #f)
)

; ===================================================================

(define-public (cog-tv-mean TV)
"
 cog-tv-mean TV
    Return the `mean` of the TruthValue TV. This is a single
    floating point-number.

    See also: cog-mean
"
	(if TV (cog-value-ref TV 0) #f)
)

(define-public (cog-tv-confidence TV)
"
 cog-tv-confidence TV
    Return the `confidence` of the TruthValue TV. This is a single
    floating point-number.

    See also: cog-confidence
"
	(if TV (cog-value-ref TV 1) #f)
)

; ===================================================================

(define-public (cog-tv ATOM)
"
 cog-tv ATOM
    Return the truth-value of ATOM. If there is no truth value,
    set on this ATOM, #t is returned. (This emulates the old behavior,
    where the default tv was true with zero confidence.)

    Example:
       ; Define a node
       guile> (define x
                 (Concept \"abc\" (SimpleTruthValue 0.2 0.5)))
       guile> (cog-tv x)
       (stv 0.2 0.5)
       guile> (cog-tv? (cog-tv x))
       #t

    See also: cog-set-tv!
"
	(define tvkey (Predicate "*-TruthValueKey-*"))
	(let ((tv (cog-value ATOM tvkey)))
		(if tv tv #t))
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
; They not like us.
;
; This is not like the other truth values.
; Keep cog-count and cog-inc-count! for a little while.
; They are still used in the learning code. They now increment a generic
; FloatValue located at the TV predicate. This should be sufficiently
; backwards compatbile that I think things will still work, over there.

(define-public (cog-inc-count! ATOM CNT)
"
  cog-inc-count! ATOM CNT -- Increment truth value on ATOM by CNT.

  Atomically increment the count on a FloatValue by CNT. The mean
  and confidence values are left untouched.  CNT may be any floating
  point number (positive or negative).

  If the current truth value on the ATOM is not a FloatValue, then
  the value is replaced by a FloatValue, with the count set to CNT.

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
		(lambda (key . args) (cog-set-value! ATOM tvkey (FloatValue 1 0 CNT))))
)

(define-public (cog-count ATOM)
"
 cog-count ATOM
    Return the `count` of the FloatValue on ATOM. This is a single
    floating point-number.

    See also: cog-tv, cog-inc-count!
"
	(cog-value-ref (cog-tv ATOM) 2))

; ===================================================================

(define-public (cog-mean ATOM)
"
 cog-mean ATOM
    Return the `mean` of the TruthValue on ATOM. This is a single
    floating point-number.

    See also: cog-confidence, cog-tv
"
	(cog-tv-mean (cog-tv ATOM)))


(define-public (cog-confidence ATOM)
"
 cog-confidence ATOM
    Return the `confidence` of the TruthValue on ATOM. This is a single
    floating point-number.

    See also: cog-mean, cog-tv
"
	(cog-tv-confidence (cog-tv ATOM)))

; ===================================================================
