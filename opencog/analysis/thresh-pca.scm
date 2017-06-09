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
; So: How should the vectors 'b' and 's' be represented? Currently, the
; intternal form will be a "wvec", consisting of a list of pairs
; (cons numeric-value  Atom), so that the first can be accessed with
; car and the second with cadr. Its also possible to provide a generic
; format, but am not doing this just right now.
;
; In the below, these vectors are called "wvec"'s.
;
; See the FAQ for why heavy-weight numerical calculations are being done
; in scheme instead of C++.
;
; ---------------------------------------------------------------------
;
(use-modules (srfi srfi-1))
(use-modules (ice-9 optargs)) ; for define*-public

; ---------------------------------------------------------------------

(define*-public (make-thresh-pca LLOBJ #:optional
	; Default is to use the pair-freq method
	(get-value 'pair-freq))
"
  make-thresh-pca LLOBJ - Do the thresholding PCA thing.

  Optionally, the name of a method can be supplied, from which the matrix
  values will be fetched.  If not supplied, it defaults to 'pair-freq,
  and so this object can be used with the default pair-freq-api object
  to work with plain-old frequencies.  But you can get fancier if yo wish.
  Using the MI could be interesting, for example: this would result in
  a MaxEnt style computation, instead of a PCA-style computation.
"
	(let ((llobj LLOBJ)
			(star-obj (add-pair-stars LLOBJ))
		)

		; --------------------
		; Return a wvec of items with uniform weighting. This is a
		; unit vector. i.e. its dot-product with itself is 1.0.

		(define (unit-wvec BASIS)
			(define weight (/ 1 (sqrt (length BASIS))))
			(map (lambda (item) (cons weight item)) BASIS))

		; Apply equal weighting to all elements of the left-basis
		; This is the starting vector for one step of left-iterate.
		(define (left-init)
			(unit-wvec (star-obj 'left-basis)))

		(define (right-init)
			(unit-wvec (star-obj 'right-basis)))

		; --------------------
		; Multiply matrix on the left by WVEC.  That is, return the
		; result wvec
		;     result(y) = sum_x p(x,y) WVEC(x)
		; Note that this is effectively the transpose of P.
		(define (left-mult LEFT-WVEC)
			(define (do-one ITEM)
				(fold
					(lambda (wcns sum)
						(+ sum
							(* (llobj get-value PAIR)
								(car wcns)
					0
					LEFT-WVEC))

			(filter-map
				(star-obj 'left-basis))))

		; Just like above, but returns the function
		;     result(x) = sum_y p(x,y) FVEC(y)
		(define (right-mult RIGHT-FVEC)
			(lambda (ITEM)
				(fold
					(lambda (PAIR sum)
						(+ sum
							(* (llobj get-value PAIR)
								(RIGHT-FVEC (gdr PAIR)))))
					0
					(star-obj 'right-stars ITEM))))

		; --------------------

		(define (left-iter-once FVEC)
			(right-mult (left-mult FVEC))
		)

		(define (right-iter-once FVEC)
			(left-mult (right-mult FVEC))
		)

		; --------------------

		; Compute the normalization of the vector; that is, compute
		; it's length.  This returns a single floating-point value.
		; Caution: it can be extremely time-consuming!
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
		; Methods on this class.
		(lambda (message . args)
			(case message
				((left-initial)           (left-init))
				((right-initial)          (right-init))
				((left-mult)              (apply left-mult args))
				((right-mult)             (apply right-mult args))
				((left-iterate)           (apply left-iter-once args))
				((right-iterate)          (apply right-iter-once args))
				((left-norm)              (apply left-norm args))
				((right-norm)             (apply right-norm args))
				(else (apply llobj        (cons message args))))))
)

; ---------------------------------------------------------------------
; ---------------------------------------------------------------------
