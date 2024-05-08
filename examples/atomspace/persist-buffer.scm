;
; persist-buffer.scm -- Using WriteBufferProxy
;
; This demo illustrates using the WriteBufferProxy to rate-limit writes
; to disk (or network) by buffering up repeated writes of the same Atom,
; and writing it out less frequently.
;
; Please review the demos in `persist-proxy.scm` for a general overview.
;
; Whenever some algorithm uses StorageNodes, it typically wants to save
; changed Atoms to the disk. It is easiest to write such algos by just
; changing the value; say, incremeneting it, and then writing. However,
; if it is the same Atom (or small set of Atoms) being constantly
; updated, this can become very inefficient. Buffering aims to solve
; this, by holding off on writes for a while, so that if a value changes
; repeatedly in a short period of time, the write is held off for a
; while, and then, later, only the most recent copy is written out.

(use-modules (opencog) (opencog persist))
(use-modules (opencog persist-rocks))

(ProxyParameters
	(WriteBufferProxy "wthru buffer")
	(RocksStorageNode "rocks:///tmp/foo.rdb")
	(Number 42))

(cog-open (WriteBufferProxy "wthru buffer"))

; Store a bunch of values in rapid succession
(for-each
	(lambda (N)
		(cog-set-value! (Concept "foo") (Predicate "bar") (FloatValue 1 2 N))
		(store-value (Concept "foo") (Predicate "bar")))
	(iota 10))

; Store the whole Atom, repeatedly
(for-each
	(lambda (N)
		(cog-set-value! (Concept "foo") (Predicate "fizz")
			(FloatValue 4 (* 5 N) (+ N 6)))
		(store-atom (Concept "foo")))
	(iota 10))

; Close the connection.
(cog-close (WriteBufferProxy "wthru buffer"))

; That's All, Folks!
; ---------------------------------------------------------------------
