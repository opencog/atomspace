;
; gram-majority.scm
;
; Merge N vectors at a time into a new cluster. Merge basis elements by
; majority democratic vote.
;
; Copyright (c) 2021 Linas Vepstas
;
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; See `gram-classification.scm` and `gram-projective.scm` for an overview.
;
; Given N vectors that have been selected to form a cluster, one can
; determine what basis elements should be a part of that cluster by
; looking to see what all the vectors have in common. If the majority
; of the vectors share a particular basis element, then all of them
; should contribute that element to the cluster.
;
; This is termed "democratic voting" since a majority concept is used,
; and each vector gets one vote. (Future extensions might consider
; proportional votes?) This idea really only works if N>2 as voting
; between two contributors does not make really make sense.
;
; TODO:
; * Reintroduce FRAC for those disjuncts not shared by the majority.
; * Maybe reintroduce NOISE for minimum counts, if this cannot be
;   handled in other ways.
;
; make-merge-majority
; -------------------
; Merge N items into a brand new cluster.  See also `make-merge-pair`
; (not in this file) which merges two items at a time, possibly into
; existing clusters.
;
; ---------------------------------------------------------------------

(use-modules (srfi srfi-1))
(use-modules (opencog) (opencog matrix) (opencog persist))

; ---------------------------------------------------------------------

; TODO: we can very easily re-introduce FRAC here, and thus
; provide compatibility with the older merge methods. Just
; modify `clique` below to do this.
;
; TODO: maybe reintroduce NOISE as well.

(define-public (make-majority-jaccard LLOBJ QUORUM)
"
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
