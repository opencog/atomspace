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

; That's it. Now, from a different terminal, start another guile shell

$ guile
scheme@(guile-user)>

(use-modules (opencog) (opencog persist))
(use-modules (opencog persist-cog))
(define sto (CogStorageNode "cog://localhost:17001"))

; Open a connection to the CogServer
(cog-open sto)

; Configure the proxy. If the List contains more than one StorageNode,
; writing will be done to all of them. We set up two, just for fun.
; But really, one is enough. For this example, we use Rocks, but they
; can be anything: Postgres, another CogServer, or another Proxy. They
; can be mixed & matched as desired.
(cog-set-proxy!
	(ProxyParameters
		(WriteThruProxy "wthru mirror")
		(List
			(RocksStorageNode "rocks:///tmp/foo.rdb")
			(RocksStorageNode "rocks:///tmp/bar.rdb"))))

; The above tells the CogServer to store data into both of the RocksDB
; instances. The name on `WriteThruProxy` Node is arbitrary; it can be
; set to anything.
;
; Now, tell the Cogserver to start using this proxy.
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

; That's it. Now we'll repeat the process, this time reading back what
; was written above. It will illustrate the ReadThruProxy.
;
; ---------------------------------------------------------------------
; Start by killing and restarting the CogServer. This "guarantees" that
; we've got no lingering data in the AtomSpace that might confuse us.
;
$ guile
scheme@(guile-user)>

(use-modules (opencog) (opencog persist))
(use-modules (opencog persist-rocks))
(use-modules (opencog cogserver))
(start-cogserver)

; That's it. Now, from a different terminal, start the client again.

$ guile
scheme@(guile-user)>

(use-modules (opencog) (opencog persist))
(use-modules (opencog persist-cog))
(define sto (CogStorageNode "cog://localhost:17001"))

; Open a connection to the CogServer
(cog-open sto)

; Configure the proxy. This time, we configure a reader. Two StorageNodes
; will be given; read requests will round-robin between them. Only one
; StorageNode is is needed; given how RocksDB works, it is almost
; certainly more efficient to use just one. Again, instead of using
; Rocks, the readers can be a mixture of anything, including other
; ProxyNodes.
;
(cog-set-proxy!
	(ProxyParameters
		(ReadThruProxy "rthru balance")
		(List
			(RocksStorageNode "rocks:///tmp/foo.rdb")
			(RocksStorageNode "rocks:///tmp/bar.rdb"))))

; Now, tell the Cogserver to start using this proxy.
(cog-proxy-open)

; Lets retreive the Atom we wrote above.
;
; Fetch the entire Atom.
(fetch-atom (Concept "foo"));

; Take a look
(cog-prt-atomspace)
(cog-keys (Concept "foo"))
(cog-value (Concept "foo") (Predicate "fizz"))

; Close the connection.
(cog-close sto)

; That's All, Folks!
; ---------------------------------------------------------------------
