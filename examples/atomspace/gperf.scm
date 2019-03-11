#!/usr/bin/env guile
!#
;
; gperf.scm -- Simple guile atom creation benchmark.
;
; This is a super-simple benchmark, with no bells & whistles. But it
; works and can give you a basic idea of how fast (or slow) atom creation
; is in the AtomSpace.  A more comprehensive set of benchmarks can be
; found in the https://github.com/opencog/benchmark repository.
;
; Run this code from the shell:
;
;     $ ./gperf.scm
;

(use-modules (opencog))
(use-modules (ice-9 format))
(use-modules (srfi srfi-19))

; Define a function that creates a link with two nodes in it.
; Things that waste CPU time here are:
; 1) conversion of number to string
; 2) concatenation of two strings
; 3) creation of the ConceptNode in the AtomSpace
; 4) creation of the ListLink in the AtomSpace.
; 5) tail recursion, to loop and repeat the process again.
(define (make-node-and-link prefix n)
	(define (make-atoms prefix node n)
		; Hmmm.. formatted printing is kind-of slow.
		;    (ConceptNode (format #f "Object_~d" n))
		; so do string-append instead.
		(define cpt (ConceptNode
			(string-append/shared prefix (number->string n))))
		(ListLink node cpt)
		(if (< 0 n)
			(make-atoms prefix cpt (- n 1))
			'()
		)
	)
	(make-atoms prefix (ConceptNode prefix) n)
)

; Define a function that creates large tree of links.
; We expect this to run much faster than the above, because it does a
; lot less.  Things that waste CPU time here are:
;   1) creation of the ListLink in the AtomSpace.
;   2) tail recursion, to loop and repeat the process again.
; This will create a tree of depth "depth"; however, since the
; atoms are unique, only "depth" of them are needed for the whole tree.
(define (make-link-tree depth)
	(define (make-atoms atom n)
		(if (< 0 n)
			(make-atoms (ListLink atom atom) (- n 1))
			'()
		)
	)
	(make-atoms (ConceptNode "nil") depth)
)

; A handy utility to report the elapsed time and the rate.
(define (report-perf id start stop niter)
	(define elapsed (time-difference stop start))
	(define delta 
		(+ (time-second elapsed) 
			(/ (time-nanosecond elapsed) 1000000000.0)))
	(define rate (round (/ niter delta)))
	(format #t "~A\n" id)
	(format #t "\tElapsed time (secs): ~A\n" delta)
	(format #t "\tLoops per second: ~A\n\n" rate)
)

(display "\nRunning the benchmark. Please wait ...\n\n")

; Measure how long it takes to create a bunch of links.
(define niter 250000)
(define start (current-time))
(make-node-and-link "hello world" niter)
(define stop (current-time))
(report-perf "Nodes and Links, first time:" start stop niter)

; Do it again. This time, its twice as fast, because the atoms already
; exist in the atomspace, and are not being made for the first time.
(set! start (current-time))
(make-node-and-link "hello world" niter)
(set! stop (current-time))
(report-perf "Nodes and Links, second time:" start stop niter)

; Make a large binary tree.
(set! start (current-time))
(make-link-tree niter)
(set! stop (current-time))
(report-perf "Binary tree, first time:" start stop niter)

; Make a large binary tree, again.
(set! start (current-time))
(make-link-tree niter)
(set! stop (current-time))
(report-perf "Binary tree, second time:" start stop niter)
