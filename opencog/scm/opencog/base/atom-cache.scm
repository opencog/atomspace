;
; atom-cache.scm
;
; Create a local cache that pairs objects to atoms.
;
; Copyright (c) 2017 Linas Vepstas
;
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; Many analytic computations require a lot of CPU-effort, and it is
; usually a big win to perform them only once, and not recompute.
; Below is a simple cache, where the atom serves as a key, allowing
; some abitrary scheme object to be associated with it.
;
; This differs from atomspace values, in several ways:
; * any arbitary scheme object can be associated with an atom.
; * these caches are never saved to the database, unlike atom values.
; * these caches are anonymous.  You must have a handle to the function
;   to make use of them.  They are automatically garbage collected.
;
; ---------------------------------------------------------------------
;
(use-modules (srfi srfi-1))
(use-modules (opencog))

; ---------------------------------------------------------------------

(define-public (make-afunc-cache AFUNC)
"
  make-afunc-cache AFUNC -- Return a caching version of AFUNC.

  Here, AFUNC is a function that takes a single atom as an argument,
  and returns some object associated with that atom.

  This returns a function that returns the same values that AFUNC would
  return, for the same argument; but if a cached value is available,
  then return just that, instead of calling AFUNC a second time.  This
  is useful whenever AFUNC is cpu-intensive, taking a long time to
  compute.  In order for the cache to be valid, the value of AFUNC must
  not depend on side-effects, because it will be called at most once.
"
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

; ---------------------------------------------------------------------

(define-public (make-atom-set)
"
  make-atom-set - Return a function that can hold set of atoms.

  This is used to maintain a set of atoms, free of duplicate entries.
  In most cases, it is faster than calling delete-duplicates! on a
  list of atoms.

  Calling the returned function with an atom in the argument places the
  atom into the set. Calling it with #f as the argument returns the
  entire set as a list.

  Example Usage:
     (define atom-set (make-atom-set))
     (atom-set (Concept \"ABC\"))
     (atom-set (Concept \"DEF\"))
     (atom-set (Concept \"ABC\"))
     (atom-set #f)

     This last call returns the list
     ((ConceptNode \"ABC\") (ConceptNode \"DEF\"))
"
	(define cache (make-hash-table))
	(define (atom-hash ATOM SZ) (modulo (cog-handle ATOM) SZ))
	(define (atom-assoc ATOM ALIST)
		(find (lambda (pr) (equal? ATOM (car pr))) ALIST))

	(lambda (ITEM)
		(if ITEM
			(hashx-set! atom-hash atom-assoc cache ITEM #f)
			(let ((ats '()))
				(hash-for-each-handle
					(lambda (PR) (set! ats (cons (car PR) ats)))
					cache)
				ats))))

; ---------------------------------------------------------------------

(define-public (delete-dup-atoms ATOM-LIST)
"
  delete-dup-atoms ATOM-LIST - Remove duplicate atoms from list.

  This does the same thing as `delete-duplicates`, but is faster.

  This will usually be faster than calling `delete-duplicates` whenever
  the ATOM-LIST is large (probably when its longer than 10 atoms long??)
"

	(define atom-set (make-atom-set))
	(for-each atom-set ATOM-LIST)
	(atom-set #f)
)

; ---------------------------------------------------------------------

(define-public (remove-duplicate-atoms ATOM-LIST)
"
  remove-duplicate-atoms ATOM-LIST - Remove duplicate atoms from list.

  This does the same thing as `delete-dup-atoms` but is slower(?)

  This will usually be faster than calling `delete-duplicates` whenever
  the ATOM-LIST is large (probably when its longer than 10 atoms long??)
"

	; Sort first, and then filter.
	(define sorted-atoms (sort ATOM-LIST cog-atom-less?))
	(if (null? sorted-atoms) '()
		(fold
			(lambda (ATM LST)
				(if (equal? ATM (car LST)) LST (cons ATM LST)))
			(list (car sorted-atoms))
			(cdr sorted-atoms)))
)

; ---------------------------------------------------------------------

(define-public (keep-duplicate-atoms ATOM-LIST)
"
  keep-duplicate-atoms ATOM-LIST - Keep only duplicate atoms from list.

  This removes all atoms in ATOM-LIST that appear only once in the list.
  The multiplicity of the remaining atoms is reduced by one, and so
  repeated calls to this function allows progressively higher
  multiplicities to be removed. For example, two calls to this function
  will cause all atoms that appear once or twice to be removed.
"
	; Sort first, and then filter.
	(define sorted-atoms (sort ATOM-LIST cog-atom-less?))
	(define head #f)

	(if (null? sorted-atoms) '()
		(begin
			(set! head (car sorted-atoms))
			(fold
				(lambda (ATM LST)
					(if (equal? ATM head)
						(cons ATM LST)
						(begin (set! head ATM) LST)))
				'()
				(cdr sorted-atoms))))
)

; ---------------------------------------------------------------------

(define-public (atoms-subtract LIST-A LIST-B)
"
  atoms-subtract LIST-A LIST-B

  Return a list of all atoms in LIST-A that are not in LIST-B.

  This does the same thing as `lset-difference` but will usually be
  much much faster, if either list is more than ten atoms long.
"
	(define cache (make-hash-table))
	(define (atom-hash ATOM SZ) (modulo (cog-handle ATOM) SZ))
	(define (atom-assoc ATOM ALIST)
		(find (lambda (pr) (equal? ATOM (car pr))) ALIST))

	(for-each (lambda (ITEM)
		(hashx-set! atom-hash atom-assoc cache ITEM #f))
		LIST-B)
	(remove (lambda (ATOM)
		(hashx-get-handle atom-hash atom-assoc cache ATOM))
		LIST-A)
)

; ---------------------------------------------------------------------
