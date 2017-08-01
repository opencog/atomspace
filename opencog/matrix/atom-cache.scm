;
; atom-cache.scm
;
; Create a local cache to pairing objects to atoms.
;
; Copyright (c) 2017 Linas Vepstas
;
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; Many of the analytic computations done here require a lot of CPU-effort;
; thus, its usually a big win to perform them only once, and not recompute.
; Thus, this implements a simple cache, where the atom serves as a key,
; and an abitrary scheme object can be associated with it.
;
; This differs from atomspace values, in several ways:
; * any arbitary scheme object can be associated with an atom
; * these caches are never saved to the database, unlike atom values.
; * these caches are anonymous.  You must have a handle to the function
;   to make use of them.  They are automatically garbage collected.
;
; ---------------------------------------------------------------------
;
(use-modules (srfi srfi-1))
(use-modules (opencog))

; ---------------------------------------------------------------------

; Return a caching version of AFUNC. Here, AFUNC is a function that
; takes a single atom as an argument, and returns some object
; associated with that atom.
;
; This returns a function that returns the same values that AFUNC would
; return, for the same argument; but if a cached value is available,
; then return just that.  In order for the cache to be valid, the AFUNC
; must be side-effect-free.
;
(define (make-afunc-cache AFUNC)

	; Define the local hash table we will use.
	(define cache (make-hash-table))

	; Guile needs help computing the hash of an atom.
	(define (atom-hash ATOM SZ) (modulo (cog-handle ATOM) SZ))
	(define (atom-assoc ATOM ALIST)
		(find (lambda (pr) (equal? ATOM (car pr))) ALIST))

	(lambda (ITEM)
		(define val (hashx-ref atom-hash atom-assoc cache ITEM))
		(if val val
			(let ((fv (AFUNC ITEM)))
				(hashx-set! atom-hash atom-assoc cache ITEM fv)
				fv)))
)
