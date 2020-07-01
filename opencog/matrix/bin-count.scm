;
; bin-count.scm
;
; Assorted ad-hoc collection of bin-counting (histogramming) tools.
; This is used to create estimates of distributions.
;
; Copyright (c) 2017 Linas Vepstas
;
(use-modules (srfi srfi-1))
(use-modules (ice-9 optargs)) ; for define*-public

; ---------------------------------------------------------------------
; ---------------------------------------------------------------------
; Bin-counting (histogramming) utilities.
;
; This must be the 20th time I've implemented bin-counts in my life...
;
(define-public (bin-count ITEMS NBINS item->value item->count
         LOWER UPPER)
"
  bin-count ITEMS NBINS item->value item->count LOWER UPPER

  Create a distribution by sorting items into bins, and then
  returning an array showing the number of items in each bin.

  The interval between UPPER and LOWER will be split into NBINS, each of
  equal size, thus having a width of (UPPER-LOWER)/NBINS. The list of
  ITEMS will be sorted into these bins according to item->value.  Each
  time an item is sorted, the bin count is incremented by item->count.

  A list of two arrays are returned: the first array holds the
  bin-centers, and the second holds the counts.

  The arguments are as follows:

  ITEMS is a list of items to be binned.

  NBINS is the number of bins to use.

  item->value is a function that, given an item, returns the score for
       that item. This score is used to determine the bin number.

  item->count should be a function that, given an item, returns the
       count or strength for that item. It should return 1 for simple
       binning, i.e. if each item should be counted exactly once.

  LOWER is the left boundary of the left-most bin; all values less than
      this will go into the left-most bin.

  UPPER is the right boundary of the right-most bin; all values greater
      than this will go into the right-most bin.
"
	(define bin-width (/ (- UPPER LOWER) NBINS))

	; Given a value, find the corresponding bin number.
	; min and max are 0 and NBINS-1
	(define (value->bin valu)
		; Check for NaN's; can happen for MI of zero counts...
		(define val (if (inf? valu) LOWER valu))
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
			(define bin (value->bin (item->value item)))
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

; The simplest-possible binning
(define*-public (bin-count-scores SCORE-LIST nbins #:optional
	; Default left and right bounds
	(min-val (min-value (lambda (x) x) SCORE-LIST))
	(max-val (max-value (lambda (x) x) SCORE-LIST)))
"
  bin-count-scores SCORE-LIST NBINS #:optional MIN MAX

  A very simple API for bin-counting. The SCORE-LIST is assumed to be
  a list of values to bin-count; that is, a list of numbers.  The
  distribution is created by sorting these values into NBINS, each of
  equal size.

  The optional arguments MIN and MAX specify the left and right
  boundaries of the bins; if a value is less than MIN, it will be put
  in the left-most bin, and values greater than MAX will go into the
  right-most bin. If MIN and MAX are not specified, then the smallest
  and largest values will be used as the endpoints.
"
	; The item is the score; so this is a no-op.
	(define (item->value item) item)

	; Increment the bin-count by this much.
	(define (item->count item) 1)

	(bin-count SCORE-LIST nbins item->value item->count
	    min-val max-val)
)

; Just use the simplest binning for scored items.
(define*-public (bin-count-simple scored-items nbins #:optional
	; Default left and right bounds
	(min-val (min-value first scored-items))
	(max-val (max-value first scored-items)))
"
  bin-count-simple SCORED-ITEMS NBINS #:optional MIN MAX

  A very simple API for bin-counting. The SCORED-ITEM is assumed to be
  a list of lists to bin-count. It is assumed that the first cell in
  each sublist holds a numeric value; this value will be used to create
  the distribution.  This is created by sorting these values into NBINS,
  each of equal size.

  The optional arguments MIN and MAX specify the left and right
  boundaries of the bins; if a value is less than MIN, it will be put
  in the left-most bin, and values greater than MAX will go into the
  right-most bin. If MIN and MAX are not specified, then the smallest
  and largest values will be used as the endpoints.
"
	; Get the item score
	(define (item->value item) (first item))

	; Increment the bin-count by this much.
	(define (item->count item) 1)

	(bin-count scored-items nbins item->value item->count
	    min-val max-val)
)

(define-public (bin-count-weighted scored-items nbins item->count)
"
  TODO this needs to be turned into a nicer API.
"
	; Get the item score
	(define (item->value item) (first item))

	; Default left and right bounds
	(define min-val (min-value item->value scored-items))
	(define max-val (max-value item->value scored-items))

	(bin-count scored-items nbins item->value item->count
	    min-val max-val)
)

(define-public (print-bincounts-tsv cent-bins port)
"
  print-bincounts-tsv BINS PORT

  Print the bin-counts held in BINS to PORT. The BINS are assumed to be
  the bin-count object, generated by the `bin-count` function. The PORT
  is assumed to be a standard scheme port, typically a file or string
  port.

  The bin-counts are printed in a tab-separated-value format, with the
  first column holding the bin number, the second holding the center of
  the bin, and the third holding the count for that bin.

  Example usage:
    (define bins ...)
    (let ((outport (open-file \"/tmp/foo.dat\" \"w\")))
        (print-bincounts-tsv bins outport)
        (close outport))
"
	(define centers (first cent-bins))
	(define bins (second cent-bins))
	(define binno 0)
	(for-each
		(lambda (bin-cnt)
			(format port "~A\t~A\t~A\n" binno (array-ref centers binno) bin-cnt)
			(set! binno (+ binno 1)))
		(array->list bins)))

; ---------------------------------------------------------------------
