;
; Simple debug tracing support.
;
; Handy utilities to write log messages to a file.
; The problem addressed here is that lots of scheme code (e.g. the
; relex2logic rules) run wrapped in C++ code, and some of that C++
; code does not correctly redirect printed output (e.g. the
; forward & backward chainers). Rather than fixing this I/O
; problem, we have a quick hack here: just dump trace messages to a
; file.  This is simple, easy, and avoids the complexity of the
; opencog logger subsystem.
;
(define oport 0)
(define dbg-cnt 0)
(define dbg-tim 0)

(define-public (init-trace file-name)
" Dump trace outfile to FILE-NAME
"
	(set! oport (open-file file-name "w"))
)

(define-public (start-trace msg)
" Start tracing, record  the start time"
	(set! dbg-tim (current-time))
	(set! dbg-cnt 0)
	(display msg oport)
	(force-output oport)
)

(define-public (trace-msg msg)
"  Print MSG to the trace logfile."
	(display msg oport)
	(force-output oport)
)

(define-public (trace-elapsed)
"  Print elapsed (wall-clock) time since trace start."
	(begin
		(display "Elapsed secs " oport)
		(display (- (current-time) dbg-tim) oport)
		(display "\n" oport)
		(set! dbg-tim (current-time))
	)
)

(define-public (trace-msg-cnt msg)
" Print message, increment the count."
	(display msg oport)
	(set! dbg-cnt (+ dbg-cnt 1))
	(display dbg-cnt oport)
	(display "\n" oport)

	; Provide some crude timing info too ...
	(if (eqv? 0 (modulo dbg-cnt 10000))
		(trace-elapsed)
	)
	(force-output oport)
)
