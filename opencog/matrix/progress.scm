;
; progress.scm
;
; Timing and progress report utilities.
;
; Copyright (c) 2013, 2014, 2017, 2018, 2021 Linas Vepstas
;
; ---------------------------------------------------------------------
;
(use-modules (srfi srfi-1))
(use-modules (ice-9 atomic))
(use-modules (ice-9 optargs))

; ---------------------------------------------------------------------

(define-public (atomic-inc ctr)
"
  atomic-inc CTR - increment the atomic-box CTR by one.

  This is the atomic version of (set! CTR (+ 1 CTR)).
  This returns the new, incremented value.

  Example usage:
     (define cnt (make-atomic-box 0))
     (atomic-inc cnt)
     (atomic-inc cnt)
     (format #t \"Its ~A\n\" (atomic-box-ref cnt))
"
	(define old (atomic-box-ref ctr))
	(define new (+ 1 old))
	(define swp (atomic-box-compare-and-swap! ctr old new))
	(if (= old swp) new (atomic-inc ctr))
)

; ---------------------------------------------------------------------

(define*-public (make-progress-rpt FUNC WHEN TOTAL MSG #:optional HYST)
"
  make-progress-rpt FUNC WHEN TOTAL MSG [HYST] - print progress report.

  This returns a wrapper around FUNC, so that it calls FUNC whenever
  it is called.

  Whenever the number of calls is a multiple of WHEN, a progress
  report is printed, as given by MSG. If the optional argument HYST
  is provided, then the progress report is printed only if at least
  HYST seconds have elapsed.

  FUNC should be the function to be called, taking one argument.
  MSG should be a string of the form
     \"Did ~A of ~A in ~A seconds (~A items/sec)\n\"
     The first argument is the message; the second is the TOTAL,
     the third is the elapsed time in seconds, the fourth is the
     rate, in calls per second. If TOTAL is #f, then this argument
     is not passed.
  WHEN should be the number of times that this function is called,
     between printing of progress reports. It is used to take the
     modulo of the of the call count.
  TOTAL should be the total number of items to process; it becomes
     the second argument to the MSG. If TOTAL is #f, then it is not
     passed to the MSG for printing.
"
	(let ((cnt (make-atomic-box 0))
			(start-time 0))
		(lambda (item)
			; back-date to avoid divide-by-zero
			(if (eqv? 0 (atomic-box-ref cnt))
				(set! start-time (- (current-time) 0.00001)))
			(FUNC item)
			(if (eqv? 0 (modulo (atomic-inc cnt) WHEN))
				(let* ((now (current-time))
						(elapsed (- now start-time))
						(ilapsed (inexact->exact (round elapsed)))
						(rate (/ (exact->inexact WHEN) elapsed))
						(irate (inexact->exact (round rate)))
						(icnt (atomic-box-ref cnt))
					)
					(when (or (not HYST) (<= HYST ilapsed))
						(if TOTAL
							(format #t MSG icnt TOTAL ilapsed irate)
							(format #t MSG icnt ilapsed irate))
						(set! start-time now))))))
)

; ---------------------------------------------------------------------

(define*-public (make-elapsed-secs #:optional hysteresis)
"
  make-elapsed-secs [hysteresis]

  Create a timer. The timer returns the number of elapsed seconds
  since the last call to it. If the optional `hysteresis` argument
  is given, then the timer is not reset until at least that many
  seconds have passed.

  Example usage:
  (define elapsed-secs (make-elapsed-secs))
  (sleep 2)
  (format #t \"Seconds since start: ~A\n\" (elapsed-secs))
"
	(define start-time (current-time))
	(lambda ()
		(define now (current-time))
		(define diff (- now start-time))
		; Reset only if the minimum has elapsed.
		(if (or (not hysteresis) (<= hysteresis diff))
			(set! start-time now))
		diff)
)

; ---------------------------------------------------------------------
