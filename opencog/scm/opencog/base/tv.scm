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
	(if EXP (cog-subtype? 'TruthValue (cog-type EXP)) #f)
)

(define-public (cog-ctv? EXP)
"
 cog-ctv? EXP
    Return #t if EXP is a CountTruthValue, else return #f.
    Equivalent to (equal? 'CountTruthValue (cog-type EXP))
"
	(if EXP (equal? 'CountTruthValue (cog-type EXP)) #f)
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

(define-public (cog-tv ATOM)
"
 cog-tv ATOM
    Return the truth-value of ATOM. If there is no truth value,
    set on this ATOM, #f is returned.

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
	(cog-value ATOM tvkey)
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

(define-public (cog-mean ATOM)
"
 cog-mean ATOM
    Return the `mean` of the TruthValue on ATOM. This is a single
    floating point-number.

    See also: cog-confidence, cog-count, cog-tv
"
	(cog-tv-mean (cog-tv ATOM)))


(define-public (cog-confidence ATOM)
"
 cog-confidence ATOM
    Return the `confidence` of the TruthValue on ATOM. This is a single
    floating point-number.

    See also: cog-mean, cog-count, cog-tv
"
	(cog-tv-confidence (cog-tv ATOM)))

(define-public (cog-count ATOM)
"
 cog-count ATOM
    Return the `count` of the TruthValue on ATOM. This is a single
    floating point-number.

    See also: cog-mean, cog-confidence, cog-tv, cog-inc-count!
"
	(cog-tv-count (cog-tv ATOM)))

; ===================================================================
