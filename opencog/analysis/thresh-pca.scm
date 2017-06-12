;
; thresh-pca.scm
;
; Perform a Thresholding Principal Component Analsysis
;
; Copyright (c) 2017 Linas Vepstas
;
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; A simple, low-brow iterative method to get a quick approximation
; to the first PCA component.  This will be used to create a kind of
; sparse PCA vector, by passing it through a sigmoid function.
; This will in turn be used identify set membership. A detailed
; description is available in the diary.
;
; Write p(x,y) for the probability of the ordered pair, as elsewhere
; throughout this code.  Write P for the index-free matrix; write P^T
; for it's transpose.  Then, given an input vector b, there are two
; kinds of iterations one can perform: a "left" and a "right" iteration.
; The "left iteration" is defined as PP^Tb and the "right iteration" as
; P^TPb.  Note that the dimension of "b" is different for these two cases,
; with dim b = right-dim of P for the right-iteration case.
;
; In index notation, a single "left iteration" step is then the
; computation of the double sum
;
;    s(w) = sum_y p(w,y) sum_x p(x,y) b(x)
;
; and the "right iteration" is
;
;    s(z) = sum_x p(x,z) sum_y p(x,y) b(y)
;
; Because P is sparse, it makes the most sense to compute the inner sum
; "on demand", for only those index values where the outer sum is
; non-vanishing. Here, "on-demand" is what scheme (or functional languages
; in general) excell in: lazy evaluation.  That is, we won't calulate
; anything until you ask for it; and it seems that because P is sparse,
; chances are good you'll never ever ask for it.
;
; So: How should the vectors 'b' and 's' be represented?  As "on demand",
; lazy-evaluation functions.  Give it an argument, that is always an Atom
; of some kind, and it will return a floating-point value ... after
; computing it.  We won't compute a value until you ask for it. (A special
; caching wrapper even avoids a computation, if its been done before).
; Thus, most of the functions below just set up other functions that
; would compute a value, if they were ever asked.
;
; In the below, these lazy vectors are called "fvec"'s.
;
; See the FAQ for why heavy-weight numerical calculations are being done
; in scheme instead of C++.
;
; ---------------------------------------------------------------------
;
(use-modules (srfi srfi-1))
(use-modules (ice-9 optargs)) ; for define*-public

; ---------------------------------------------------------------------

(define*-public (make-power-iter-pca LLOBJ #:optional
	; Default is to use the pair-freq method
	(get-value 'pair-freq))
"
  make-power-iter-pca LLOBJ - Implement a power-iteration form of PCA.

  Optionally, the name of a method can be supplied, from which the matrix
  values will be fetched.  If not supplied, it defaults to 'pair-freq,
  and so this object can be used with the default freq-api object to work
  with plain-old frequencies.  But you can get fancier if you wish.
  Using the MI could be interesting, for example: this would result in
  a MaxEnt style computation, instead of a PCA-style computation.
"
	(let ((llobj LLOBJ)
			(star-obj (add-pair-stars LLOBJ))
		)

		; --------------------
		; Return an fvec of items with uniform weighting. This is a
		; unit vector. i.e. its dot-product with itself is 1.0.

		; What this actually returns is a function, that when
		; called with any argument, returns a constant.
		(define (unit-fvec BASIS-SIZE)
			(define weight (/ 1 (sqrt BASIS-SIZE)))

			(lambda (ITEM) weight))

		; Apply equal weighting to all elements of the left-basis
		; This is the starting vector for one step of left-iterate.
		(define (left-init)
			(unit-fvec (star-obj 'left-basis-size)))

		(define (right-init)
			(unit-fvec (star-obj 'right-basis-size)))

		; --------------------
		; Multiply matrix on the left by FVEC.  That is, return the
		; function
		;     result(y) = sum_x p(x,y) FVEC(x)
		; As always, this returns the function `result`. Call this
		; function with an arguement to force the computation to
		; happen.  Note that this is effectively the transpose of P.
		(define (left-mult LEFT-FVEC)

			; We are going to cache it, because we know we will hit it hard.
			(define fvec (make-afunc-cache LEFT-FVEC))
			(lambda (ITEM)
				(fold
					(lambda (PAIR sum)
						(+ sum
							(* (llobj get-value PAIR)
								(fvec (gar PAIR)))))
					0
					(star-obj 'left-stars ITEM))))

		; Just like above, but returns the function
		;     result(x) = sum_y p(x,y) FVEC(y)
		(define (right-mult RIGHT-FVEC)
			(define fvec (make-afunc-cache RIGHT-FVEC))
			(lambda (ITEM)
				(fold
					(lambda (PAIR sum)
						(+ sum
							(* (llobj get-value PAIR)
								(fvec (gdr PAIR)))))
					0
					(star-obj 'right-stars ITEM))))

		; --------------------
		; Compute the normalization of the vector; that is, compute
		; it's length.  This returns a single floating-point value.
		; Caution: it's time-consuming, because it runs over the
		; entire left-dimension, when it's invoked.
		(define (left-norm FVEC)
			(define start (current-time))
			(define sumsq
				(fold
					(lambda (item sum)
						(define val (FVEC item))
						(+ sum (* val val)))
					0
					(star-obj 'left-basis)))
			(format #t "left-norm took ~d seconds\n" (- (current-time) start))
			(sqrt sumsq))

		(define (right-norm FVEC)
			(define start (current-time))
			(define sumsq
				(fold
					(lambda (item sum)
						(define val (FVEC item))
						(+ sum (* val val)))
					0
					(star-obj 'right-basis)))
			(format #t "right-norm took ~d seconds\n" (- (current-time) start))
			(sqrt sumsq))

		; --------------------
		; Renormalize the vector.  Given the input FVEC, return an
		; fvec that is of unit length.
		(define (left-renormalize FVEC)
			(define norm #f)
			; Look we gotta call FVEC at least once; we may as well
			; cache the result.
			(define fvec (make-afunc-cache FVEC))
			(lambda (ITEM)
				(if (not norm) (set! norm (/ 1 (left-norm fvec))))
				(* norm (fvec ITEM))))

		; Same as above.
		(define (right-renormalize FVEC)
			(define norm #f)
			(define fvec (make-afunc-cache FVEC))
			(lambda (ITEM)
				(if (not norm) (set! norm (/ 1 (right-norm fvec))))
				(* norm (fvec ITEM))))

		; --------------------

		; Perform a single step of power-iteration.
		(define (left-iter-once FVEC)
			(left-renormalize (right-mult (left-mult FVEC))))

		(define (right-iter-once FVEC)
			(right-renormalize (left-mult (right-mult FVEC))))

		; Perform K steps of power-iteration.
		(define (left-iter FVEC K)
			(if (>= 0 K) FVEC
				(left-iter (left-iter-once FVEC) (- K 1))))

		(define (right-iter FVEC K)
			(if (>= 0 K) FVEC
				(right-iter (right-iter-once FVEC) (- K 1))))

		; --------------------
		; Get an explicit vector, formatted as a list of pairs,
		; each pair being (item . value)
		(define (get-fvec FVEC BASIS)
			(map
				(lambda (item) (cons item (FVEC item)))
				(star-obj BASIS)))

		(define (get-left-vec FVEC) (get-fvec FVEC 'left-basis))
		(define (get-right-vec FVEC) (get-fvec FVEC 'right-basis))

		; --------------------
		; Print the top-K values of the vector
		(define (print-fvec FVEC K BASIS)
			(define start (current-time))
			(define sorted-vals
				(sort
					(get-fvec FVEC BASIS)
					(lambda (a b) (> (cdr a) (cdr b)))))

			(for-each
				(lambda (item) (format #t "~A\n" item))
				(take sorted-vals K))
			(format #t "get-fvec took ~d seconds\n" (- (current-time) start))
		)

		(define (left-print FVEC K) (print-fvec FVEC K 'left-basis))
		(define (right-print FVEC K) (print-fvec FVEC K 'right-basis))

		; --------------------
		; Methods on this class.
		(lambda (message . args)
			(case message
				((left-initial)      (left-init))
				((right-initial)     (right-init))
				((left-mult)         (apply left-mult args))
				((right-mult)        (apply right-mult args))
				((left-iterate)      (apply left-iter args))
				((right-iterate)     (apply right-iter args))
				((left-norm)         (apply left-norm args))
				((right-norm)        (apply right-norm args))
				((left-renormalize)  (apply left-renormalize args))
				((right-renormalize) (apply right-renormalize args))
				((left-vec)          (apply get-left-vec args))
				((right-vec)         (apply get-right-vec args))
				((left-print)        (apply left-print args))
				((right-print)       (apply right-print args))
				(else                (apply llobj (cons message args))))))
)

; ---------------------------------------------------------------------

(define-public (make-cosine-matrix LLOBJ)
"
  make-cosine-matrix LLOBJ - Provide a cosine-matrix form of LLOBJ.

  Given an LLOBJ whose 'pair-freq returns values p(x,y), one can define
  another matrix such that the rows or columns are normailized to be unit
  vectors.  That is, one can define the left-unit

     L(x,y) = p(x,y) / sqrt(sum_u p^2(u,y))

  which has the property that L(x,y) is a vector of unit length when y is
  treated as a paramter (i.e. when y is held fixed).  The dot-product of
  two unit-length vectors is just the cosine of the angle between them, and
  so this can be used to construct the left cosine-similarity as

     sim(y_1, y_2) = sum_x L(x, y_1) L(x, y_2)

  We call it the "left similarity" to emphasize that the summation is
  taking place over the left index.

  The LLOBJ object needs to provide the 'pair-freq method.
"
	(let* ((star-obj (add-pair-stars LLOBJ))
			(supp-obj (add-support-compute star-obj))
		)

		; --------------------
		(define (do-left-unit PAIR)
			(define frq (LLOBJ 'pair-freq PAIR))
			(define len (supp-obj 'left-length (gdr PAIR)))
			(/ frq len)
		)

		(define (do-right-unit PAIR)
			(define frq (LLOBJ 'pair-freq PAIR))
			(define len (supp-obj 'right-length (gar PAIR)))
			(/ frq len)
		)

		(define cache-left-unit (make-afunc-cache do-left-unit))
		(define cache-right-unit (make-afunc-cache do-right-unit))

		; --------------------
		; Methods on this class.
		(lambda (message . args)
			(case message
				((left-unit)         (apply cache-left-unit args))
				((right-unit)        (apply cache-right-unit args))
				(else                (apply llobj (cons message args))))))
)

; ---------------------------------------------------------------------
; ---------------------------------------------------------------------
