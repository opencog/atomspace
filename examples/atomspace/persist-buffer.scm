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
; default time constant T0 is 60 seconds; this means that most writes in
; a 60-second window will be buffered up, but about exp(-60/60)=exp(-1)=0.22
; of them will be actually written out.
;
; ---------------------------------------------------------------------

(use-modules (opencog) (opencog persist))
(use-modules (opencog persist-rocks))

; The default write buffer size is 60 seconds; change it to 42. This
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

; View performance stats. (But these will be zero, because 42 seconds
; haven't passed by yet. The average rate is also rounded to the nearest
; integer, and so will round down to zero.)
(display (monitor-storage (WriteBufferProxy "write buffer")))

; There are also perf stats for the base server:
(display (monitor-storage (RocksStorageNode "rocks:///tmp/foo.rdb")))

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

; ---------------------------------------------------------------------
; The WriteBuffer only buffers writes. It completely ignores reads.
; In most cases, one wants to both read and write. This is arranged
; for by using the ReadWriteProxy, to gang together one reader and
; one writer.

(ProxyParameters
	(ReadWriteProxy "read w/write buffer")
	(List
		(RocksStorageNode "rocks:///tmp/foo.rdb")   ;; target for reads
		(WriteBufferProxy "write buffer")))         ;; target for writes

(cog-open (ReadWriteProxy "read w/write buffer"))
(fetch-atom (Concept "foo"))
(cog-set-value! (Concept "foo") (Predicate "fizz") (Concept "oh hi"))
(store-atom (Concept "foo"))
(cog-close (ReadWriteProxy "read w/write buffer"))

; One can get fancier: the reader always goes to the target to read
; Atoms and Values. The CachingProxy will cache reads, avoiding a
; fetch from storage, if the given Atom/Value is already in the
; AtomSpace. The can be ganged up with the write-buffer, to offer
; caching both ways.

(ProxyParameters
	(CachingProxy "read cache")
	(RocksStorageNode "rocks:///tmp/foo.rdb"))

(ProxyParameters
	(ReadWriteProxy "full cache")
	(List
		(CachingProxy "read cache")           ;; target for reads
		(WriteBufferProxy "write buffer")))   ;; target for writes

(cog-open (ReadWriteProxy "full cache"))
(fetch-atom (Concept "foo"))
(cog-set-value! (Concept "foo") (Predicate "fizz") (Number 42))
(store-atom (Concept "foo"))

; Look at the cache statistics:
(display (monitor-storage (ReadWriteProxy "full cache")))

(cog-close (ReadWriteProxy "full cache"))

; That's All, Folks!
; ---------------------------------------------------------------------
