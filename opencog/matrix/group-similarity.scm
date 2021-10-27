;
; group-similarity.scm
;
; Provide similarity scores for N vectors.
;
; Copyright (c) 2021 Linas Vepstas
;
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; Given N vectors, one may wish to determine how similar they are to
; one-another. One can, of course, compute pair-wise similarities, but
; there are N!/2 such pairs, and so this gets quickly out of hand.
; Besides, its not clear how to combine pair-wise similarities. Thus,
; it is handy to have similarity measures that work for the entire
; group, taken as a whole.  This file provides that.
;
; Currently, only one function is provided: a generalized Jaccard
; similarity. It is generalized in that it works for N vectors, and not
; just two. It is also generalized by replacing the Jaccard "min"
; function by a "democratic vote" function, where an item is accepted
; if it is shared in commmon by a majority.
;
; ---------------------------------------------------------------------

(use-modules (srfi srfi-1))
(use-modules (opencog) (opencog persist))

; ---------------------------------------------------------------------

(define-public (make-group-similarity LLOBJ)
"
  make-group-similarity LLOBJ - Extend LLOBJ with methods to compute
  similarities between rows or columns of the LLOBJ sparse matrix. This
  is a generalization of pair-wise similarity to the case of finding the
  mutual similarity btween three or more rows/columns in the matrix. If
  only pair-wise similarity is needed, use the `add-similarity-compute`
  object.

  Currently, this only provides a generalized Jaccard distance. This
  generalizes the conventional Jaccard distance by preplacing the "min"
  function by a "democratic vote" function, where an item is accepted
  if it is shared in commmon by a majority.

  Some terminology: Let D(x,y) be 1 if the matrix element (x,y) exists
  (i.e. has a non-zero count N(x,y)) and be otherwise.

  make-majority-jaccard LLOBJ QUORUM -- Return a function that
  counts the number of connector sequences shared by a majority
  of the words in a word list. The majority is determined by QUORUM,
  which should be a floating-point number between 0.0 and 1.0.

  When the returned function is invoked on a list of words, it returns
  a list of two numbers: the number of connector sequences shared by
  the majority, and the total number number of connector sequences that
  appear on the words.

  This returns a function that implements a kind-of Jaccard distance
  between multiple words (two or more).  The conventional Jaccard
  distance is defined only for pairs of items. The generalization is
  done by counting to see if a fraction QUORUM is shared. Setting
  QUORUM to 1.0, and appplying the function to two items returns
  the conventional Jaccard distance.
"
	; WLIST is a list of WordNodes and/or WordClassNodes that are
	; being proposed for merger. This will count how many disjuncts
	; these share in common. It returns a pair of numbers: the number
	; of disjuncts shared, and the total disjuncts.
	(define (count WLIST)

		; The minimum number of sections that must exist for
		; a given disjunct. For a list of length two, both
		; must share that disjunct (thus giving the traditional
		; overlap merge).
		(define wlen (length WLIST))
		(define vote-thresh
			(if (equal? wlen 2) 2
				(inexact->exact (round (* QUORUM wlen)))))

		; Return #t if the DJ is shared by the majority of the
		; sections. Does the count exceed the threshold?
		(define (vote-to-accept? DJ)
			(<= vote-thresh
				(fold
					(lambda (WRD CNT)
						(if (nil? (LLOBJ 'get-pair WRD DJ)) CNT (+ 1 CNT)))
					0
					WLIST)))

		; Put all of the connector-sets on all of the words int a bag.
		(define set-of-all-djs (make-atom-set))
		(for-each
			(lambda (WRD)
				(for-each
					(lambda (DJ) (set-of-all-djs DJ))
					(LLOBJ 'right-basis WRD)))
			WLIST)

		(define list-of-all-djs (set-of-all-djs #f))

		; Count the particular DJ, if it is shared by the majority.
		(define shared-count
			(fold
				(lambda (DJ CNT)
					(if (vote-to-accept? DJ) (+ 1 CNT) CNT))
				list-of-all-djs))

		; Return two numbers: the shared count and the total count.
		(list shared-count (length list-of-all-djs))
	)

	; Return the above function
	count
)

; ---------------------------------------------------------------
; Example usage  (none)
