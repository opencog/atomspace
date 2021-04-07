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
; (use-modules (ice-9 threads))

; ---------------------------------------------------------------------

(define-public (atomic-inc ctr)
"
  atomic-inc CTR - increment the atomic-boc CTR by one.

  This is the atomic version of (set! CTR (+ 1 CTR)).
  This returns the new, incremented value.

  Example usage:
     (define cnt (make-atomic-box 0))
     (atomic-inc cnt)
     (atomic-inc cnt)
     (format #t "Its ~A\n" (atomic-box-ref cnt))
"
	(define old (atomic-box-ref ctr))
	(define new (+ 1 old))
	(define swp (atomic-box-compare-and-swap! ctr old new))
	(if (= old swp) new (atomic-inc ctr))
)

; ---------------------------------------------------------------------

(define-public (make-progress-rpt FUNC WHEN TOTAL MSG)
"
  make-progress-rpt FUNC WHEN TOTAL MSG - print progress report.

  This returns a wrapper around FUNC, so that it calls FUNC whenever
  it is called.

  Whenever the number of calls is a multiple of WHEN, a progress
  report is printed, as given by MSG.

  FUNC should be the function to be called, taking one argument.
  MSG should be a string of the form
     \"Did ~A of ~A in ~A seconds (~A items/sec)\n\"
     The first argument is the message; the second is the TOTAL,
     the third is the elapsed time in seconds, the fourth is the
     rate, in calls per second.
  WHEN should be how often to print (modulo)
  TOTAL should be the total number of items to process; it becomes
     the second argument to the MSG.
"
	(let ((func FUNC)
			(when WHEN)
			(total TOTAL)
			(msg MSG)
			(cnt (make-atomic-box 0))
			(start-time 0))
		(lambda (item)
			; back-date to avoid divide-by-zero
			(if (eqv? 0 cnt) (set! start-time (- (current-time) 0.00001)))
			(func item)
			(if (eqv? 0 (modulo (atomic-inc cnt) when))
				(let* ((elapsed (- (current-time) start-time))
						(ilapsed (inexact->exact (round elapsed)))
						(rate (/ (exact->inexact when) elapsed))
						(irate (inexact->exact (round rate)))
					)
					(format #t msg (atomic-box-ref cnt) total ilapsed irate)
					(set! start-time (current-time))))))
)

; ---------------------------------------------------------------------

; Create a timer. The timer returns the number of elapsed seconds
; since the last call to it.
;
; Example usage:
; (define elapsed-secs (make-elapsed-secs))
; (sleep 2)
; (format #t "Seconds since start: ~A\n" (elapsed-secs))

(define (make-elapsed-secs)
	(define start-time (current-time))
	(lambda ()
		(define now (current-time))
		(define diff (- now start-time))
		(set! start-time now)
		diff)
)

; ---------------------------------------------------------------------
