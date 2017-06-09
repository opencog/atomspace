;
; cosine.scm
;
; Define object-oriented class API's for computing the cosine and
; jaccard distances.
;
; Copyright (c) 2017 Linas Vepstas
;
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; See object-api.scm for the overview.  Or the README.md file.
; ---------------------------------------------------------------------

(use-modules (srfi srfi-1))
(use-modules (ice-9 optargs)) ; for define*-public

; ---------------------------------------------------------------------

(define*-public (add-pair-cosine-compute LLOBJ
	#:optional (GET-CNT (lambda (x) (LLOBJ 'pair-count x))))
"
  add-pair-cosine-compute LLOBJ - Extend LLOBJ with methods to compute
  vector dot-products and cosine angles.  None of these use cached
  values, instead, they compute these values on the fly.

  Some terminology: Let N(x,y) be the observed count for the pair (x,y).
  There are two ways of computing a dot-product: summing on the left, or
  the right.  Thus we define the left-product as
      left-prod(y,z) = sum_x N(x,y) N(x,z)
  and the right-product as
      right-prod(x,u) = sum_y N(x,y) N(u,y)

  Similarly, we can define the left and right cosine angles as
      left-cosine(y,z) = left-prod(y,z) /
             (left-length(y) * left-length(z))
  and likewise for the right side. Recall that the left-length is
  defined as
      left-length(y) = sqrt sum_x N(x,y) N(x,y)
                     = sqrt left-prod(y,y)

  The Jaccard distance can be defined as one minus the Jaccard
  similarity, which is defined as

      left-jacc-sim(y,z) = sum_x min (N(x,y), N(x,z)) /
               sum_x max (N(x,y), N(x,z))

  Here, the LLOBJ is expected to be an object, with valid
  counts associated with each pair. LLOBJ is expected to have
  working, functional methods for 'left-type and 'right-type
  on it.

  By default, the N(x,y) is taken to be the 'get-count method
  on LLOBJ, i.e. it is literally the count. The optional argument
  GET-CNT allows this to be over-ridden with any other method
  that returns a number.  For example, to compute the lengths
  and norms for frequencies, pass this lambda as the second
  argument:
     (lambda (x) ((add-pair-freq-api LLOBJ) 'pair-freq x))
  Any function that takes a pair and returns a number is allowed.
"

	; Min and max of individual elements
	(define (mintu TUPLE)  (min (first TUPLE) (second TUPLE)))
	(define (maxtu TUPLE)  (max (first TUPLE) (second TUPLE)))

	(let ((llobj LLOBJ)
			(star-obj (add-pair-stars LLOBJ))
			(supp-obj (add-support-api LLOBJ))
			(min-obj  (add-support-compute
				(add-tuple-math LLOBJ mintu GET-CNT)))
			(max-obj  (add-support-compute
				(add-tuple-math LLOBJ maxtu GET-CNT)))
			(get-cnt GET-CNT))

		; -------------
		; Given the low-level pair LOPR, return the numeric count for it.
		(define (get-lo-cnt LOPR)
			(get-cnt (llobj 'item-pair LOPR)))

		; Compute the dot-product, summing over items from the
		; LIST, (which are pairs containing ITEM-A) and using the
		; get-other-lopr function to get the other pair to sum over
		; (i.e. replacing ITEM-A in the list with ITEM-B)
		; With a suitable LIST and get-other-lopr, this can do
		; either the left or the right sums.
		(define (compute-product get-other-lopr ITEM-B LIST)
			; Loop over the the LIST
			(fold
				(lambda (lopr sum)
					(define a-cnt (get-lo-cnt lopr))
					(define b-pr (get-other-lopr lopr ITEM-B))

					(if (null? b-pr)
						sum
						(+ sum (* a-cnt (get-lo-cnt b-pr)))))
				0
				LIST))

		; Return the low-level pair (x,y) if it exists, else
		; return the empty list '()
		(define (have-lopr? X Y)
			(cog-link (llobj 'pair-type) X Y))

		; Get the "other pair", for lefty wild
		(define (get-other-left LOPR OTHER)
			(have-lopr? (gar LOPR) OTHER))

		; Get the "other pair", for righty wild
		(define (get-other-right LOPR OTHER)
			(have-lopr? OTHER (gdr LOPR)))

		(define (compute-left-product ITEM-A ITEM-B)
			(compute-product get-other-left ITEM-B
				(star-obj 'left-stars ITEM-A)))

		(define (compute-right-product ITEM-A ITEM-B)
			(compute-product get-other-right ITEM-B
				(star-obj 'right-stars ITEM-A)))

		; -------------
		; Return the cosine of the left-angle between ITEM-A and B.
		; The cosine as defined above.
		(define (compute-left-cosine ITEM-A ITEM-B)
			(define prod (compute-left-product ITEM-A ITEM-B))
			(define deno (*
				(supp-obj 'left-length ITEM-A)
				(supp-obj 'left-length ITEM-B)))
			(if (eqv? 0.0 deno) 0.0 (/ prod deno)))

		; As above, but for the right.
		(define (compute-right-cosine ITEM-A ITEM-B)
			(define prod (compute-right-product ITEM-A ITEM-B))
			(define deno (*
				(supp-obj 'right-length ITEM-A)
				(supp-obj 'right-length ITEM-B)))
			(if (eqv? 0.0 deno) 0.0 (/ prod deno)))

		; -------------
		; Return the left-jaccard distance
		(define (compute-left-jaccard-dist ITEM-A ITEM-B)
			(define left-min (min-obj 'left-count (list ITEM-A ITEM-B)))
			(define left-max (max-obj 'left-count (list ITEM-A ITEM-B)))
			(- 1.0 (/ left-min left-max))
		)

		; Return the right-jaccard distance
		(define (compute-right-jaccard-dist ITEM-A ITEM-B)
			(define right-min (min-obj 'right-count (list ITEM-A ITEM-B)))
			(define right-max (max-obj 'right-count (list ITEM-A ITEM-B)))
			(- 1.0 (/ right-min right-max))
		)

	; -------------
	; Methods on this class.
	(lambda (message . args)
		(case message
			((left-product)    (apply compute-left-product args))
			((right-product)   (apply compute-right-product args))
			((left-cosine)     (apply compute-left-cosine args))
			((right-cosine)    (apply compute-right-cosine args))
			((left-jaccard)    (apply compute-left-jaccard-dist args))
			((right-jaccard)   (apply compute-right-jaccard-dist args))
			(else (apply llobj (cons message args))))
		)))

; ---------------------------------------------------------------------
