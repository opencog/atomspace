;
; persist-proxy.scm -- Using proxy agents to automate storage.
;
; This demo illustrates how to use proxy agents to move Atoms around
; from one storage location to another. The primary demo is of a network
; server that is serving Atoms from off the disk. The idea is that when
; the server is started, it's AtomSpace is empty, and all of the Atoms
; are sitting on disk. When the first client attaches and asks for some
; Atom, the server has to go and grab it from the disk, first. The
; `ReadThruProxyNode` can do this: when it sees that request, it turns
; around and passes it forward to the disk StorageNode. Once fetched
; from disk, it can then be returned by network to the requesting client.
;
; Proxy agents currently include:
;
; * ReadThruProxy -- Passes on requests involving the reading of
;      Atoms and Values. This includes `fetch-atom`, `fetch-value`,  
;      `fetch-incoming-set` and `fetch-incoming-by-type`. This is
;      a load-balancing or proxy: if there are multiple providers,
;      it will pick one and use that for a given request.
;
; * SequentialReadProxy -- Pass read requests on to the first
;      StorageNode. If that suceeds, return that. Else try the
;      next in line, until there either the Atom/Value is found,
;      or until the list is exhaused.
;
; * CachingProxy -- If an Atom or Value is already in the AtomSpace,
;      do nothing. Otherwise go to the StorageNode to get it.
;      Experimental. The currrent implementation is minimal, and
;      does nothing to limit cache size or expire old data.
;
; * WriteThruProxy -- Passes on requests involving the storing of
;      Atoms and Values. This includes `store-atom`, `store-value`,
;      and `store-referrers`. This is a mirroring proxy: if there
;      is more than one target, the write will be made too all of
;      them.
;
; * ReadWriteProxy -- Combines reading and writing into one. It does
;      NOT do mirroring or load-balancing; do get that, just chain in
;      the above!
;
; * DynamicDataProxy -- Create FormulaStreams and FutureStreams on
;      the fly. Experimental.
;
; ---------------------------------------------------------------------
; The below illustrates a simple WriteThruProxy. It starts with a
; CogServer attached to an empty RocksDB database. Then a network
; client stores some data into it.
;
; First, start guile, and then the CogServer.

$ guile
scheme@(guile-user)>

(use-modules (opencog) (opencog persist))
(use-modules (opencog persist-rocks))
(use-modules (opencog cogserver))
(start-cogserver)

; That's it. Now, from a different terminal
