;
; bin-count.scm
;
; Assorted ad-hoc collection of bin-counting tools. This is used to
; create estimates of distributions.
;
; Copyright (c) 2017 Linas Vepstas
;
(use-modules (srfi srfi-1))
(use-modules (ice-9 optargs)) ; for define*-public

; ---------------------------------------------------------------------
; ---------------------------------------------------------------------
; Bin-counting utilities.
;
; This must be the 20th time I've implemented bin-counts in my life...
;
; ITEMS is a list of items to be binned.
;
; NBINS is the number of bins to use.
;
; item->value is a function that, given an item, returns the score for
;      that item. This score is used to determine the bin number.
;
; item->count should be a function that, given an item, returns the
;      count or strength for that item. It should return 1 for simple
;      binning.
;
; LOWER is the left boundry of the left-most bin; all values less than
;     this will go into the left-most bin.
;
; UPPER is the right boundry of the right-most bin; all values greater
;     than this will go into the right-most bin.

(define-public (bin-count ITEMS NBINS item->value item->count
         LOWER UPPER)

	(define bin-width (/ (- UPPER LOWER) NBINS))

	; Given a value, find the corresponding bin number.
	; min and max are 0 and NBINS-1
	(define (value->bin val)
		(define bino
			(inexact->exact (floor (/ (- val LOWER) bin-width))))
		(if (< 0 bino)
			(if (> NBINS bino) bino (- NBINS 1))
			0))

	(define bins (make-array 0 NBINS))
	(define centers (make-array 0 NBINS))

	; Do the actual bin-counting
	(for-each
		(lambda (item)
			(define bin (value->bin (car item)))
			(array-set! bins
				(+ (array-ref bins bin) (item->count item))
				bin))
		ITEMS)

	; Store the centers of the bins
	(array-index-map! centers
		(lambda (i) (+ (* (+ i 0.5) bin-width) LOWER)))

	(list centers bins)
)

; Find the smallest value in the list of ITEMS.
; The item->value function returns the value, given the item.
(define (min-value item->value ITEMS)
	(fold
		(lambda (item min)
			(define value (item->value item))
			(if (< min value) min value))
		1e300
		ITEMS))


; Find the largest value in the list of ITEMS.
; The item->value function returns the value, given the item.
(define (max-value item->value ITEMS)
	(fold
		(lambda (item max)
			(define value (item->value item))
			(if (> max value) max value))
		-1e300
		ITEMS))

; Just use the simplest binning
(define*-public (bin-count-simple scored-items nbins #:optional
	; default left andright bounds
	(min-val (min-value first scored-items))
	(max-val (max-value first scored-items)))

	; Get the item score
	(define (item->value item) (first item))

	; Increment the bin-count by this much.
	(define (item->count item) 1)

	(bin-count scored-items nbins item->value item->count
	    min-val max-val)
)

(define-public (bin-count-weighted scored-items nbins item->count)
	; Get the item score
	(define (item->value item) (first item))

	; Default left and right bounds
	(define min-val (min-value item->value scored-items))
	(define max-val (max-value item->value scored-items))

	(bin-count scored-items nbins item->value item->count
	    min-val max-val)
)


(define (print-ts-bincounts cent-bins port)
	(define centers (first cent-bins))
	(define bins (second cent-bins))
	(define binno 0)
	(for-each
		(lambda (bin-cnt)
			(format port "~A	~A	~A\n" binno (array-ref centers binno) bin-cnt)
			(set! binno (+ binno 1)))
		(array->list bins)))

; ---------------------------------------------------------------------
