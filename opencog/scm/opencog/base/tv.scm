;
; tv.scm
;
; Useful utilities for working with truth values.
;
; Utilities provided:
; -- cog-merge-tv! -- merge truth values on atom
; -- cog-merge-hi-conf-tv! -- different merge style
; -- cog-stv-strength -- SimpleTruthValue strength of an atom
; -- cog-stv-strength-above -- Filter atoms with TV strength above a threshold
; -- cog-stv-strength-below -- Filter atoms with TV strength below a threshold
; -- cog-stv-confidence -- TruthValue confidence of an atom
; -- cog-stv-confidence-above -- Filter atoms with TV confidence above a threshold
; -- cog-stv-confidence-below -- Filter atoms with TV confidence below a threshold
; -- cog-stv-count -- TruthValue count of an atom
; -- cog-stv-count-above -- Filter atoms with TV count above a threshold
; -- cog-stv-count-below -- Filter atoms with TV count below a threshold
; -- cog-stv-positive-filter -- Filter atoms with positive TV strength and count
;
;
; Copyright (c) 2014 Cosmo Harrigan
;

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

(define-public (cog-stv? EXP)
"
 cog-stv? EXP
    Return #t if EXP is a SimpleTruthValue, else return #f.
    Equivalent to (equal? 'SimpleTruthValue (cog-type EXP))
"
	(equal? 'SimpleTruthValue (cog-type EXP))
)

(define-public (cog-ctv? EXP)
"
 cog-ctv? EXP
    Return #t if EXP is a CountTruthValue, else return #f.
    Equivalent to (equal? 'CountTruthValue (cog-type EXP))
"
	(equal? 'CountTruthValue (cog-type EXP))
)

(define-public (cog-itv? EXP)
"
 cog-itv? EXP
    Return #t if EXP is a IndefiniteTruthValue, else return #f.
    Equivalent to (equal? 'IndefiniteTruthValue (cog-type EXP))
"
	(equal? 'IndefiniteTruthValue (cog-type EXP))
)

(define-public (cog-ptv? EXP)
"
 cog-ptv? EXP
    Return #t if EXP is a ProbablisticTruthValue, else return #f.
    Equivalent to (equal? 'ProbabilisticTruthValue (cog-type EXP))
"
	(equal? 'ProbabilisticTruthValue (cog-type EXP))
)

(define-public (cog-ftv? EXP)
"
 cog-ftv? EXP
    Return #t if EXP is a FuzzyTruthValue, else return #f.
    Equivalent to (equal? 'FuzzyTruthValue (cog-type EXP))
"
	(equal? 'FuzzyTruthValue (cog-type EXP))
)

; ===================================================================
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
(define-public (cog-stv-strength x)
"
  cog-stv-strength
  Return the truth value strength of an atom
  (Compatible with atoms that have a SimpleTruthValue)
"
	(cdr (assoc 'mean (cog-tv->alist (cog-tv x)))))

; -----------------------------------------------------------------------
(define-public (cog-stv-strength-above y z)
"
  cog-stv-strength-above
  Given a threshold 'y' and a list of atoms 'z', returns a list of atoms with
  truth value strength above the threshold
  (Compatible with atoms that have a SimpleTruthValue)
"
	(filter (lambda (x) (> (cog-stv-strength x) y)) z))

; -----------------------------------------------------------------------
(define-public (cog-stv-strength-below y z)
"
  cog-stv-strength-below
  Given a threshold 'y' and a list of atoms 'z', returns a list of atoms with
  truth value strength above the threshold
  (Compatible with atoms that have a SimpleTruthValue)
"
	(filter (lambda (x) (< (cog-stv-strength x) y)) z))

; -----------------------------------------------------------------------
(define-public (cog-stv-confidence x)
"
  cog-stv-confidence
  Return the truth value confidence of an atom
  (Compatible with atoms that have a SimpleTruthValue)
"
	(cdr (assoc 'confidence (cog-tv->alist (cog-tv x)))))

; -----------------------------------------------------------------------
(define-public (cog-stv-confidence-above y z)
"
  cog-stv-confidence-above
  Given a threshold 'y' and a list of atoms 'z', returns a list of atoms with
  truth value confidence above the threshold
  (Compatible with atoms that have a SimpleTruthValue)
"
	(filter (lambda (x) (> (cog-stv-confidence x) y)) z))

; -----------------------------------------------------------------------
(define-public (cog-stv-confidence-below y z)
"
  cog-stv-confidence-below
  Given a threshold 'y' and a list of atoms 'z', returns a list of atoms with
  truth value confidence above the threshold
  (Compatible with atoms that have a SimpleTruthValue)
"
	(filter (lambda (x) (< (cog-stv-confidence x) y)) z))

; -----------------------------------------------------------------------
(define-public (cog-stv-count x)
"
  cog-stv-count
  Return the truth value count of an atom
  (Compatible with atoms that have a SimpleTruthValue)
"
	(cdr (assoc 'count (cog-tv->alist (cog-tv x)))))

; -----------------------------------------------------------------------
(define-public (cog-stv-count-above y z)
"
  cog-stv-count-above
  Given a threshold 'y' and a list of atoms 'z', returns a list of atoms with
  truth value count above the threshold
  (Compatible with atoms that have a SimpleTruthValue)
"
	(filter (lambda (x) (> (cog-stv-count x) y)) z))

; -----------------------------------------------------------------------
(define-public (cog-stv-count-below y z)
"
  cog-stv-count-below
  Given a threshold 'y' and a list of atoms 'z', returns a list of atoms with
  truth value count above the threshold
  (Compatible with atoms that have a SimpleTruthValue)
"
	(filter (lambda (x) (< (cog-stv-count x) y)) z))

; -----------------------------------------------------------------------
(define-public (cog-stv-positive-filter x)
"
  cog-stv-positive-filter
  Given a list of atoms, returns a list containing the subset that has
  truth value count > 0 and truth value strength > 0
  (Compatible with atoms that have a SimpleTruthValue)
"
	(cog-stv-strength-above 0 (cog-stv-count-above 0 x)))
