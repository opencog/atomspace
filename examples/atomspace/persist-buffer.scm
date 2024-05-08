;
; persist-buffer.scm -- Using WriteBufferProxy
;
; This demo illustrates the use of the WriteBufferProxy to coalesce
; rapid repeated writes of the same Atom, so as to reduce overall
; disk or network traffic.
;
; Please review the demos in `persist-proxy.scm` for a general overview.
;
; The easiest way to write Atomese algos that do stuff and store to disk,
; is to just do stuff, and store to disk, and not worry about it. This
; will typically result in some small set of Atoms being written to disk,
; over and over, at a very fast rate. This can be a performance bottleneck.
;
; The WriteBufferProxy can be used to eliminate this bottlneck. This is
; generally much easier than redesigning an algo to write less often.
;
; The WriteBufferProxy uses a time-delay mechanism to write out a portion
; of the buffer over time. It uses an exponential decay strategy, so that
; a portion exp(-t/T0) of the buffer is written out after t seconds. The
; default time constant T0 is 30 seconds; this means that most writes in
; a 30-second window will be buffered up, but about exp(-30/30)=exp(-1)=0.22
; of them will be actually written out.

(use-modules (opencog) (opencog persist))
(use-modules (opencog persist-rocks))

; The default write buffer size is 30 seconds; change it to 42. This
; parameter is optional, and can be skipped.
(ProxyParameters
	(WriteBufferProxy "write buffer")
	(RocksStorageNode "rocks:///tmp/foo.rdb")
	(Number 42))

(cog-open (WriteBufferProxy "write buffer"))

; Store a bunch of values in rapid succession.
(for-each
	(lambda (N)
		(cog-set-value! (Concept "foo") (Predicate "bar") (FloatValue 1 2 N))
		(store-value (Concept "foo") (Predicate "bar")))
	(iota 10))

; The buffer will drain itself, eventually. The drain can be forced with
; the (barrier) command. For example:
(barrier)

; Store the whole Atom, repeatedly.
(for-each
	(lambda (N)
		(cog-set-value! (Concept "foo") (Predicate "fizz")
			(FloatValue 4 (* 5 N) (+ N 6)))
		(store-atom (Concept "foo")))
	(iota 10))

; Close the connection. This will automatically flush the buffer,
; before returning.
(cog-close (WriteBufferProxy "write buffer"))

; That's All, Folks!
; ---------------------------------------------------------------------
