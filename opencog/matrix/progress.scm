;
; progress.scm
;
; (Private) Timing and progress report utilities.
;
; Copyright (c) 2013, 2014, 2017, 2018 Linas Vepstas
;
; ---------------------------------------------------------------------
;
(use-modules (srfi srfi-1))
(use-modules (ice-9 atomic))
; (use-modules (ice-9 threads))

; ---------------------------------------------------------------------
; ---------------------------------------------------------------------
; A progress report utility.
; This wraps the FUNC function, and prints a progress report MSG
; every WHEN calls to FUNC.
; FUNC should be the function to be called, taking one argument.
; MSG should be a string of the form
;    "Did ~A of ~A in ~A seconds (~A items/sec)\n"
; WHEN should be how often to print (modulo)
; TOTAL should be the total number of items to process.

(define (make-progress-rpt FUNC WHEN TOTAL MSG)
	(let ((func FUNC)
			(when WHEN)
			(total TOTAL)
			(msg MSG)
			(cnt 0)
			(start-time 0))
		(lambda (item)
			; back-date to avoid divide-by-zero
			(if (eqv? 0 cnt) (set! start-time (- (current-time) 0.00001)))
			(func item)
			(set! cnt (+ 1 cnt))
			(if (eqv? 0 (modulo cnt when))
				(let* ((elapsed (- (current-time) start-time))
						(ilapsed (inexact->exact (round elapsed)))
						(rate (/ (exact->inexact when) elapsed))
						(irate (inexact->exact (round rate)))
					)
					(format #t msg cnt total ilapsed irate)
					(set! start-time (current-time))))))
)

; ---------------------------------------------------------------------

; Create a timer. The timer returns teh number of elapsed seconds
; since the last call to it.
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
