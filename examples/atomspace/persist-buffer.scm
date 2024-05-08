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

$ guile
scheme@(guile-user)>

(use-modules (opencog) (opencog persist))
(use-modules (opencog persist-rocks))

(cog-set-proxy!
	(ProxyParameters
		(WriteBufferProxy "wthru mirror")
		(List
			(RocksStorageNode "rocks:///tmp/foo.rdb"))))

; ... start using this proxy.
(cog-proxy-open)

; Now store some stuff. You might want to do a `(cog-prt-atomspace)`
; back at the CogServer, just to see what's going on there.
;
; Store the whole Atom
(store-atom (Concept "foo" (stv 0.3 0.6)))

; Store a single Value.
(cog-set-value! (Concept "foo") (Predicate "bar") (FloatValue 1 2 3))
(store-value (Concept "foo") (Predicate "bar"))

; Store the whole Atom
(cog-set-value! (Concept "foo") (Predicate "fizz") (FloatValue 4 5 6))
(store-atom (Concept "foo"))

; Close the connection.
(cog-close sto)

; That's All, Folks!
; ---------------------------------------------------------------------
