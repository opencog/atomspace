;
; OpenCog Persistence module
; Copyright (C) 2009 Linas Vepstas <linasvepstas@gmail.com>
;

(define-module (opencog persist))

(use-modules (ice-9 optargs)) ; for define*-public

(use-modules (opencog))
(use-modules (opencog as-storage-config))
(load-extension (string-append opencog-ext-path-persist "libpersist")
	"opencog_persist_init")

; And another one, for force the shlib ctor to run.
(load-extension (string-append opencog-ext-path-persist-flow "libpersist-flow")
	"opencog_persist_flow_init")

; And another one, for force the shlib ctor to run.
(load-extension (string-append opencog-ext-path-persist-proxy "libpersist-proxy")
	"opencog_persist_proxy_init")

(include-from-path "opencog/persist/types/storage_types.scm")

;; Global. Maybe should be per-fluid? I dunno, this is for backwards
;; compat, mostly. New users should use messages, directly.
(define *-current-storage-node-* #f)

(define-public (cog-storage-node)
"
 cog-storage-node

    Return the currently open StorageNode. Returns an invalid handle
	 if the previously-open StorageNode was closed.  If there are
    multiple open connections, this will return only the first one
    to have been opened. If it has been closed, this will return
    an invalid handle, even if others may have been opened later.

    See also:
       `cog-open` to open a connection.
       `cog-close` to close a connection.
       `cog-connected?` to obtain the connection status.
       `monitor-storage` to print connection information.
"
	*-current-storage-node-*
)

;; -----------------------------------------------------
;;
(define-public (*-close-*)
"
  (Predicate \"*-close-*\") message

    Close an open connection. Closing the connection disables further
	 communication on the connection.

    Examples:
       (cog-set-value!
			(PostgresStorage \"postgres:///example-db?user=foo&password=bar\")
			(*-close-*) (VoidValue))
       (cog-set-value!
			(RocksStorage \"rocks:///tmp/my-rocks-db\")
			(*-close-*) (VoidValue))
       (cog-set-value!
			(CogserverStorage \"cog://localhost:17001\")
			(*-close-*) (VoidValue))

    See also:
       `*-open-*` to open a connection.
       `*-connected?-*` to obtain the connection status.
       `*-monitor-*` to print connection information.
"
	(PredicateNode "*-close-*")
)

(define*-public (cog-close #:optional (STORAGE (cog-storage-node)))
"
 cog-close [STORAGE]

    Convenience wrapper around the `*-close-*` message.

    Close an open connection to STORAGE. Closing the connection
    disables further communication on the connection. The optional
    STORAGE indicates which connection to close; if not specified,
    the currently open connection will be closed.

    Examples:
       (cog-close (PostgresStorage \"postgres:///example-db?user=foo&password=bar\"))
       (cog-close (RocksStorage \"rocks:///tmp/my-rocks-db\"))
       (cog-close (CogserverStorage \"cog://localhost:17001\"))

    See also:
       `cog-open` to open a connection.
       `cog-connected?` to obtain the connection status.
       `cog-storage-node` to obtain the current connection.
       `monitor-storage` to print connection information.
"
	(if STORAGE
		(direct-setvalue! STORAGE (*-close-*) (VoidValue)))
	(set! *-current-storage-node-* #f)
)

(define-public (*-open-*)
"
  (Predicate \"*-open-*\") message

    Open a connection to the indicated STORAGE-ATOM. An open connection
    allows Atoms to be sent/received along this connection.

    Examples:
       (cog-set-value!
			(PostgresStorage \"postgres:///example-db?user=foo&password=bar\")
			(*-open-*) (VoidValue))
       (cog-set-value!
			(RocksStorage \"rocks:///tmp/my-rocks-db\")
			(*-open-*) (VoidValue))
       (cog-set-value!
			(CogserverStorage \"cog://localhost:17001\")
			(*-open-*) (VoidValue))

    See also:
       `*-close-*` to close a connection.
       `*-connected?-*` to obtain the connection status.
       `*-monitor-*` to print connection information.
"
	(PredicateNode "*-open-*")
)

(define-public (cog-open STORAGE)
"
 cog-open STORAGE-ATOM

    Convenience wrapper around the `*-open-*` message.

    Open a connection to the indicated STORAGE-ATOM. An open connection
    allows Atoms to be sent/received along this connection.

    Examples:
       (cog-open (PostgresStorage \"postgres:///example-db?user=foo&password=bar\"))
       (cog-open (RocksStorage \"rocks:///tmp/my-rocks-db\"))
       (cog-open (CogserverStorage \"cog://localhost:17001\"))

    See also:
       `cog-close` to close a connection.
       `cog-connected?` to obtain the connection status.
       `cog-storage-node` to obtain the current connection.
       `monitor-storage` to print connection information.
"
	(if *-current-storage-node-*
		(direct-setvalue! STORAGE (*-close-*) (VoidValue)))

	(direct-setvalue! STORAGE (*-open-*) (VoidValue))
	(set! *-current-storage-node-* STORAGE)
)

(define-public (*-connected?-*)
"
  (Predicate \"*-connected?-*\") message

    Used to query the open/close status of the STORAGE-ATOM.
    Return (BoolValue 1) if there is an open connection to STORAGE-ATOM.
    Connections are opened with `*-open-*` and closed with `*-close-*`.

    See also:
       `*-open-*` to open a connection.
       `*-close-*` to close a connection.
       `*-monitor-*` to print connection information.
"
	(PredicateNode "*-connected?-*")
)

(define*-public (cog-connected? #:optional (STORAGE (cog-storage-node)))
"
 cog-connected? [STORAGE]

    Convenience wrapper around the `*-connected?-*` message.

    Return #t if there is an open connection to STORAGE.
    Connections are opened with `cog-open` and closed with `cog-close`.

    See also:
       `cog-open` to open a connection.
       `cog-close` to close a connection.
       `cog-storage-node` to obtain the current connection.
       `monitor-storage` to print connection information.
"
	(if STORAGE
		(let ((boo (cog-value STORAGE (*-connected?-*))))
			(not (eq? 0 (cog-value-ref boo 0))))
		#f)
)

(define-public (*-fetch-atom-*)
"
  (PredicateNode "*-fetch-atom-*") message.

  Fetch all of the Values on one or more ATOMs from storage.

    This updates (clobbers) all of the values in the atomspace,
    and replaces them with the ones fetched from storage.

    Usage:
       (cog-set-value! (StorageNode ...) (*-fetch-atom-*) ATOM)
       (cog-set-value! (StorageNode ...) (*-fetch-atom-*)
                       (LinkValue ATOMSPACE ATOM1 ATOM2 ...))

    See also:
    *-fetch-value-* -- to get only one Value.
    *-store-atom-* -- to store all Values.
"
	(PredicateNode "*-fetch-atom-*")
)

(define*-public (fetch-atom ATOM #:optional (STORAGE (cog-storage-node)))
"
 fetch-atom ATOM [STORAGE]

    Convenience wrapper around the `*-fetch-atom-*` message.

    If the optional STORAGE argument is provided, then it will be
    used as the source of the fetch. It must be a StorageNode.

    See also:
       `fetch-value` to get only one Value.
       `store-atom` to store all Values.
"
	; Need to use direct-setvalue! here, instead of cog-set-value! in order
	; to get correct behavior for working with frames. This is due to
	; an unpatched bug: calling cog-set-value! in some frame where
	; the storage node is only in a lower frame causes a COW of that
	; StorageNode into that frame. But the COW is a copy, and that
	; copy isn't actually open. So direct-setvalue! just bypasses the COW
	; and everything works. I'm not sure what the best long-term fix
	; for this is. At any rate, XXX FIXME.
	;
	; For backwards compat, throw a C++ exception if not open.
	(if STORAGE
		(direct-setvalue! STORAGE (*-fetch-atom-*)
			(LinkValue (cog-atomspace) ATOM))
		(throw 'C++-EXCEPTION fetch-atom "StorageNode is not open for reading!"))
	ATOM
)

(define-public (*-fetch-value-*)
"
  (PredicateNode "*-fetch-value-*") message.

  Fetch from storage one or more Values on an ATOM.

    This updates (clobbers) any current Values stored at the KEYs,
    replacing them with the ones fetched from storage.

    Usage:
       (cog-set-value! (StorageNode ...) (*-fetch-value-*)
                       (LinkValue ATOM KEY))
       (cog-set-value! (StorageNode ...) (*-fetch-value-*)
                       (LinkValue ATOM KEY1 KEY2 ...))
       (cog-set-value! (StorageNode ...) (*-fetch-value-*)
                       (LinkValue ATOMSPACE ATOM KEY1 KEY2 ...))

    See also:
    *-fetch-atom-* -- to get all Values.
    *-store-value-* -- to store only one Value.
"
	(PredicateNode "*-fetch-value-*")
)

(define*-public (fetch-value ATOM KEY #:optional (STORAGE (cog-storage-node)))
"
 fetch-value ATOM KEY [STORAGE]

    Convenience wrapper around the `*-fetch-value-*` message.

    If the optional STORAGE argument is provided, then it will be
    used as the source of the fetch. It must be a StorageNode.

    See also:
       `fetch-atom` to get all Values.
       `store-value` to store only one Value.
"
	(direct-setvalue! STORAGE (*-fetch-value-*)
		(LinkValue (cog-atomspace) ATOM KEY))
	ATOM
)

(define-public (*-fetch-incoming-set-*)
"
  (PredicateNode "*-fetch-incoming-set-*") message.

  Fetch the incoming set of one or more ATOMs from storage.

    The fetch is NOT recursive. See `load-referrers` for a recursive
    fetch.

    Usage:
       (cog-set-value! (StorageNode ...) (*-fetch-incoming-set-*) ATOM)
       (cog-set-value! (StorageNode ...) (*-fetch-incoming-set-*)
                       (LinkValue ATOMSPACE ATOM1 ATOM2 ...))

    See also:
    *-load-referrers-* -- to get every graph that contains an Atom.
    *-fetch-incoming-by-type-* -- to fetch a subset of a given type.
    *-fetch-query-* -- to fetch a query-defined collection of Atoms.
"
	(PredicateNode "*-fetch-incoming-set-*")
)

(define*-public (fetch-incoming-set ATOM #:optional (STORAGE (cog-storage-node)))
"
 fetch-incoming-set ATOM [STORAGE]

    Convenience wrapper around the `*-fetch-incoming-set-*` message.

    If the optional STORAGE argument is provided, then it will be
    used as the source of the fetch. It must be a StorageNode.

    See also:
      `load-referrers` to get every graph that contains an Atom.
      `fetch-incoming-by-type` to fetch a subset of a given type.
      `fetch-query` to fetch a query-defined collection of Atoms.
"
	(direct-setvalue! STORAGE (*-fetch-incoming-set-*)
		(LinkValue (cog-atomspace) ATOM))
	ATOM
)

(define-public (*-fetch-incoming-by-type-*)
"
  (PredicateNode "*-fetch-incoming-by-type-*") message.

  Fetch those links of the incoming set that are of specific types.

    This is a more limited fetch than the one done by
    `*-fetch-incoming-set-*` and can be useful when the incoming set
    is large. Can process multiple atom-type pairs in one call.

    Usage:
       (cog-set-value! (StorageNode ...) (*-fetch-incoming-by-type-*)
                       (LinkValue ATOM (TypeNode TYPE)))
       (cog-set-value! (StorageNode ...) (*-fetch-incoming-by-type-*)
                       (LinkValue ATOM1 (TypeNode TYPE1) ATOM2 (TypeNode TYPE2) ...))
       (cog-set-value! (StorageNode ...) (*-fetch-incoming-by-type-*)
                       (LinkValue ATOMSPACE ATOM1 (TypeNode TYPE1) ATOM2 (TypeNode TYPE2) ...))

    See also:
    *-fetch-incoming-set-* -- to fetch all of the incoming set.
    *-fetch-query-* -- to fetch a query-defined collection of Atoms.
"
	(PredicateNode "*-fetch-incoming-by-type-*")
)

(define*-public (fetch-incoming-by-type ATOM TYPE #:optional (STORAGE (cog-storage-node)))
"
 fetch-incoming-by-type ATOM TYPE [STORAGE]

    Convenience wrapper around the `*-fetch-incoming-by-type-*` message.

    If the optional STORAGE argument is provided, then it will be
    used as the source of the fetch. It must be a StorageNode.

    See also:
      `load-referrers` to get every graph that contains an Atom.
      `fetch-incoming-set` to fetch all of the incoming set.
      `fetch-query` to fetch a query-defined collection of Atoms.
"
	(direct-setvalue! STORAGE (*-fetch-incoming-by-type-*)
		(LinkValue (cog-atomspace) ATOM (TypeNode TYPE)))
	ATOM
)

(define-public (*-store-atom-*)
"
  (PredicateNode \"*-store-atom-*\") message.

  Store one or more Atoms and all of their associated keys and values
  to storage.

    This updates (clobbers) the values previously held in storage,
    replacing them by the values in the atomspace.

    Usage:
       (cog-set-value! (StorageNode ...) (*-store-atom-*) ATOM)
       (cog-set-value! (StorageNode ...) (*-store-atom-*)
                       (LinkValue ATOM1 ATOM2 ...))

    See also:
    *-store-value-* -- to store one (or more) Values.
    *-fetch-atom-* -- to fetch all Values on an Atom.
"
	(PredicateNode "*-store-atom-*")
)

(define*-public (store-atom ATOM #:optional (STORAGE (cog-storage-node)))
"
 store-atom ATOM [STORAGE]

    Store indicated ATOM, and all of its associated keys and values,
    to storage. This updates (clobbers) the values previously held
    in storage, replacing them by the values in the atomspace.

    If the optional STORAGE argument is provided, then it will be
    used as the target of the store. It must be a StorageNode.

    See also:
       `store-value` to store just one Value.
       `fetch-atom` to fetch all Values on an Atom.
"
	; Need to use direct-setvalue! here, instead of cog-set-value! in order
	; to get correct behavior for working with frames. This is due to
	; an unpatched bug: calling cog-set-value! in some frame where
	; the storage node is only in a lower frame causes a COW of that
	; StorageNode into that frame. But the COW is a copy, and that
	; copy isn't actually open. So direct-setvalue! just bypasses the COW
	; and everything works. I'm not sure what the best long-term fix
	; for this is. At any rate, XXX FIXME.
	;
	; For backwards compat, throw a C++ exception if not open.
	(if STORAGE
		(direct-setvalue! STORAGE (*-store-atom-*) ATOM)
		(throw 'C++-EXCEPTION cog-open "StorageNode is not open for writing!"))
	ATOM
)

(define-public (*-store-value-*)
"
  (PredicateNode \"*-store-value-*\") message.

  Store a single Value on an Atom.

    This will store the Value located at KEY on ATOM. This updates
    (clobbers) the Value previously held in storage, replacing it by
    the Value in the atomspace.

    Usage:
       (cog-set-value! (StorageNode ...) (*-store-value-*)
                       (LinkValue ATOM KEY))

    See also:
    *-update-value-* -- perform an atomic read-modify-write.
    *-store-atom-* -- store all values on an Atom.
    *-fetch-value-* -- fetch just one Value.
"
	(PredicateNode "*-store-value-*")
)

(define*-public (store-value ATOM KEY #:optional (STORAGE (cog-storage-node)))
"
 store-value ATOM KEY [STORAGE]

    Convenience wrapper around the `*-store-value-*` message.

    If the optional STORAGE argument is provided, then it will be
    used as the target of the store. It must be a StorageNode.

    See also:
       `update-value` to perform an atomic read-modify-write.
       `store-atom` to store all values on an Atom.
       `fetch-value` to fetch just one Value.
"
	(direct-setvalue! STORAGE (*-store-value-*) (LinkValue ATOM KEY))
)

(define-public (*-update-value-*)
"
  (PredicateNode \"*-update-value-*\") message.

  Update a Value on an Atom.

    Update the Value located at KEY on ATOM, folding in DELTA. This
    performs an atomic read-modify-write of the Value located at KEY.

    At this time, the only implemented updates are atomic increments
    of floating-point values stored in a FloatValue or in a TruthValue.
    The intended use is to allow lots of clients to simultaneously
    update counts on ATOM, without having them clobber each-other with
    a non-atomic update.

    Usage:
       (cog-set-value! (StorageNode ...) (*-update-value-*)
                       (LinkValue ATOM KEY DELTA))

    See also:
    *-store-value-* -- store a single value on an Atom.
    *-store-atom-* -- store all values on an Atom.
    *-fetch-value-* -- fetch just one Value.
"
	(PredicateNode "*-update-value-*")
)

(define*-public (update-value ATOM KEY DELTA #:optional (STORAGE (cog-storage-node)))
"
 update-value ATOM KEY DELTA [STORAGE]

    Convenience wrapper around the `*-update-value-*` message.

    If the optional STORAGE argument is provided, then it will be
    used as the target of the store. It must be a StorageNode.

    See also:
       `store-value` to store a single value on an Atom.
       `store-atom` to store all values on an Atom.
       `fetch-value` to fetch just one Value.
"
	(direct-setvalue! STORAGE (*-update-value-*) (LinkValue ATOM KEY DELTA))
)

(define-public (*-load-atoms-of-type-*)
"
  (PredicateNode \"*-load-atoms-of-type-*\") message.

  Load atoms of a specific type from storage.

    Fetch atoms of the given TYPE from storage. This fetches the
    atoms, and all the associated values attached to them.

    Usage:
       (cog-set-value! (StorageNode ...) (*-load-atoms-of-type-*) TYPE)

    Where TYPE is a TypeNode.

    See also:
    *-fetch-atom-* -- fetch an individual ATOM, and all Values on it.
    *-load-atomspace-* -- Load all atoms.
    *-load-frames-* -- load DAG of AtomSpaces.
"
	(PredicateNode "*-load-atoms-of-type-*")
)

(define*-public (load-atoms-of-type TYPE #:optional (STORAGE (cog-storage-node)))
"
 load-atoms-of-type TYPE [STORAGE]

    Convenience wrapper around the `*-load-atoms-of-type-*` message.

    If the optional STORAGE argument is provided, then it will be
    used as the source of the load. It must be a StorageNode.

    See also:
    fetch-atom ATOM -- fetch an individual ATOM, and all Values on it.
    *-load-atomspace-* -- Load all atoms.
    *-load-frames-* -- load DAG of AtomSpaces.
"
	(direct-setvalue! STORAGE (*-load-atoms-of-type-*) (TypeNode TYPE))
)

(define-public (*-load-atomspace-*)
"
  (PredicateNode \"*-load-atomspace-*\") message.

  Load the entire AtomSpace from storage.

    This will load the ENTIRE contents of the storage into the current
    AtomSpace. Depending on the size of the storage, this may take a lot
    of time.  During normal operation, a bulk-load is rarely required, as
    individual atoms can always be fetched, one at a time.

    Usage:
       (cog-set-value! (StorageNode ...) (*-load-atomspace-*) (AtomSpace))

    See also:
    *-store-atomspace-* -- store the entire AtomSpace to storage.
    *-load-frames-* -- load the DAG of AtomSpaces from storage.
    *-fetch-atom-* -- fetch an individual ATOM, and all Values on it.
    *-fetch-query-* -- get all Atoms for a given QUERY.
    *-load-referrers-* -- get every graph that contains ATOM.
    *-load-atoms-of-type-* -- load only atoms of type TYPE.
"
	(PredicateNode "*-load-atomspace-*")
)

(define*-public (load-atomspace #:optional (ATOMSPACE #f) (STORAGE #f))
"
 load-atomspace [ATOMSPACE] [STORAGE] - load all atoms from storage.

    Convenience wrapper around *-load-atomspace-*.

    If the optional ATOMSPACE argument is provided, then the data will
    be restored to that AtomSpace, instead of the current AtomSpace.

    If the optional STORAGE argument is provided, then it will be
    used as the source of the load. It must be a StorageNode.

    See also:
    fetch-atom ATOM -- fetch an individual ATOM, and all Values on it.
    fetch-incoming-set ATOM -- fetch the entire incoming set of ATOM.
    fetch-incoming-by-type ATOM TYPE -- get a subset of the incoming set.
    fetch-query QUERY -- get all Atoms for a given QUERY.
    load-referrers ATOM -- get every graph that contains ATOM.
    load-atoms-of-type TYPE -- load only atoms of type TYPE.
    load-frames -- load DAG of AtomSpaces.
    store-atomspace -- store all Atoms in the AtomSpace.
"
	; Sort out which of two optional args showed up.
	(if (and ATOMSPACE (cog-subtype? 'StorageNode (cog-type ATOMSPACE)))
		(begin
			(set! STORAGE ATOMSPACE)
			(set! ATOMSPACE (cog-atomspace))))

	(if (not ATOMSPACE) (set! ATOMSPACE (cog-atomspace)))
	(if (not STORAGE) (set! STORAGE (cog-storage-node)))
	(direct-setvalue! STORAGE (*-load-atomspace-*) ATOMSPACE)
)

(define-public (*-store-atomspace-*)
"
  (PredicateNode \"*-store-atomspace-*\") message.

  Store the entire AtomSpace to storage.

    This will dump the ENTIRE contents of the current AtomSpace to the
    the currently-open storage. Depending on the size of the AtomSpace,
    this may take a lot of time. During normal operation, a bulk-save
    is rarely required, as individual atoms can always be stored, one
    at a time.

    If the current AtomSpace sits on top of a stack of AtomSpaces, then
    only the shallowest visible Atoms in the current AtomSpace will be
    stored. Atoms that have been deleted in the current Atomspaces but
    are present in deeper AtomSpaces will NOT be stored. Values in
    deeper AtomSpaces that are hidden/changed in the current AtomSpace
    will NOT be stored. In other words, the only Atoms and Values that
    are stored are those that are visible in the current AtomSpace.

    Usage:
       (cog-set-value! (StorageNode ...) (*-store-atomspace-*) ATOMSPACE)

    See also:
    *-load-atomspace-* -- load the entire AtomSpace from storage.
    *-store-frames-* -- store the DAG of AtomSpaces to storage.
    *-store-atom-* -- store an individual Atom.
"
	(PredicateNode "*-store-atomspace-*")
)

(define*-public (store-atomspace #:optional (ATOMSPACE #f) (STORAGE #f))
"
 store-atomspace [ATOMSPACE] [STORAGE] - Store all atoms to storage.

    Convenience wrapper around the `*-store-atomspace-*` message.

    If the optional ATOMSPACE argument is provided, then it will be
    stored, instead of the current AtomSpace.

    If the optional STORAGE argument is provided, then it will be
    used as the target of the store. It must be a StorageNode.

    See also:
    load-atomspace -- load all Atoms in the AtomSpace.
    store-atom ATOM -- store one ATOM and all of the values on it.
    store-referrers ATOM -- store all graphs that contain ATOM.
"
	; Sort out which of two opional args showed up.
	(if (and ATOMSPACE (cog-subtype? 'StorageNode (cog-type ATOMSPACE)))
		(begin
			(set! STORAGE ATOMSPACE)
			(set! ATOMSPACE (cog-atomspace))))

	(if (not ATOMSPACE) (set! ATOMSPACE (cog-atomspace)))
	(if (not STORAGE) (set! STORAGE (cog-storage-node)))
	(direct-setvalue! STORAGE (*-store-atomspace-*) ATOMSPACE)
)

;
; --------------------------------------------------------------------
(define-public (*-fetch-query-*)
"
  (PredicateNode \"*-fetch-query-*\") message.

  Perform a query at the storage server and load results.

    Perform the QUERY at the storage server, and load the results into
    the AtomSpace. The results will be returned directly and also cached
    at KEY. The QUERY must be either a JoinLink, MeetLink or QueryLink.

    This can be thought of as a generalization of load-referrers,
    fetch-incoming-set and fetch-incoming-by-type. Thus, the
    simplest JoinLink is effectively the same thing as load-referrers,
    while a JoinLink with a depth of one is the same thing as
    fetch-incoming-set, and a JoinLink with a type restriction is the
    same thing as fetch-incoming-by-type.

    Usage:
       (cog-set-value! (StorageNode ...) (*-fetch-query-*)
                       (LinkValue QUERY KEY))
       (cog-set-value! (StorageNode ...) (*-fetch-query-*)
                       (LinkValue QUERY KEY METADATA FRESH))
       (cog-set-value! (StorageNode ...) (*-fetch-query-*)
                       (LinkValue ATOMSPACE QUERY KEY METADATA FRESH))

    Where FRESH is either (TrueLink) for true or (FalseLink) for false.

    See also:
    *-fetch-incoming-set-* -- to fetch the incoming set of an Atom.
    *-load-referrers-* -- to fetch all graphs containing an Atom.
"
	(PredicateNode "*-fetch-query-*")
)

(define*-public (fetch-query QUERY KEY
	#:optional (METADATA '()) (FRESH #f) (STORAGE #f))
"
 fetch-query QUERY KEY [METADATA [FRESH]] [STORAGE]

   Perform the QUERY at the storage server, and load the results into
   the AtomSpace. The results will be returned directly and also cached
   at KEY. The QUERY must be either a JoinLink, MeetLink or QueryLink.

   This can be thought of as a generalization of `load-referrers`,
   `fetch-incoming-set` and `fetch-incoming-by-type`. Thus, the
   simplest JoinLink is effectively the same thing as `load-referrers`,
   while a JoinLink with a depth of one is the same thing as
   `fetch-incoming-set`, and a JoinLink with a type restriction is the
   same thing as `fetch-incoming-by-type`.

   The storage server is free to return previously cached results for
   the query, instead of running it freshly. The storage server is
   free to refuse to perform the search.

   The METADATA Atom is optional.  If it is specified, then metadata
   about the search results is placed on QUERY at the key METADATA.
   This may include a time-stamp of when the search was performed,
   or an indicator that the search was refused (was not performed.)

   The FRESH boolean value is optional. If set to #t, then it is
   a request to the storage server to re-run the query, instead of
   returning previously cached results. The storage server may not
   honor this request; status is returned in METADATA.

   Not all storage servers are capable of performing all queries.
   If a storage server is unable to perform a query, the status
   is placed at the METADATA key.

   See also:
     `fetch-incoming-set` to fetch the incoing set of an Atom.
     `load-referrers` to fetch all graphs containing an Atom.
"
	; Handle the different parameter combinations
	(if (nil? METADATA)
		; Simple case: just QUERY and KEY
		(begin
			(if (not STORAGE) (set! STORAGE (cog-storage-node)))
			(direct-setvalue! STORAGE (*-fetch-query-*)
				(LinkValue (cog-atomspace) QUERY KEY)))
		; METADATA provided
		(if (cog-subtype? METADATA 'StorageNode)
			; oh wait; METADATA is actually a StorageNode
			(direct-setvalue! METADATA (*-fetch-query-*)
				(LinkValue (cog-atomspace) QUERY KEY))
			; METADATA is a real metadata handle
			(begin
				(if (not STORAGE) (set! STORAGE (cog-storage-node)))
				(direct-setvalue! STORAGE (*-fetch-query-*)
					(LinkValue (cog-atomspace) QUERY KEY METADATA
						(if FRESH (TrueLink) (FalseLink)))))))
	QUERY
)

; --------------------------------------------------------------------
(define*-public (store-referrers ATOM #:optional (STORAGE #f))
"
 store-referrers ATOM [STORAGE] -- Store all hypergraphs that contain ATOM

    This stores all hypergraphs that the ATOM participates in.
    It does this by recursively exploring the incoming set of the atom.

    If the optional STORAGE argument is provided, then it will be
    used as the target of the store. It must be a StorageNode.

    See also `load-referrers`.
"
	(define (do-store atom)
		(let ((iset (cog-incoming-set atom)))
			(if (null? iset)
				(store-atom atom STORAGE)
				(for-each do-store iset)
			)
		)
	)
	(do-store ATOM)
)

; --------------------------------------------------------------------
(define*-public (load-referrers ATOM #:optional (STORAGE #f))
"
 load-referrers ATOM [STORAGE] -- Load all graphs that contain ATOM.

   This loads all hypergraphs that the given ATOM participates in.
   It does this by recursively exploring the incoming set of the atom.

   If the optional STORAGE argument is provided, then it will be
   used as the source of the load. It must be a StorageNode.

   See also:
     `fetch-incoming-set` to fetch only the first level above an Atom.
     `fetch-query` to perform a generalized query for holders of an Atom.
     `store-referrers` to store all referrers.
"
	(if (not (null? ATOM))
		; The fetch-incoming-set function for this is defined to perform
		; a recursive fetch.
		; We do an extra recursion here, in case we were passed a list.
		(begin
			(if (pair? ATOM)
				(for-each load-referrers ATOM STORAGE)
				(fetch-incoming-set ATOM STORAGE)
			)
			(for-each
				(lambda (atm) (load-referrers atm STORAGE))
				(cog-incoming-set ATOM))
		)
	)
)

; --------------------------------------------------------------------
; Renamed functions
(define-public (cog-delete ATOM) "See cog-delete!" (cog-delete! ATOM))
(define-public (cog-delete-recursive ATOM)
	"See cog-delete-recursive!" (cog-delete-recursive! ATOM))


(define-public (*-delete-*)
"
  (PredicateNode \"*-delete-*\") message.

    Remove one or more Atoms from Storage, but only if they have
    no incoming links.  If it has incoming links, the remove fails.

    Use `*-delete-recursive-*` to force removal, together with any
    links that might be holding these atoms.

    A word of caution about multi-threaded operation: if one thread is
    adding atoms, while another thread is removing the *same* atoms at
    the same time, then these two threads will race. As a result of
    this racing, there is no gauranteee that the AtomSpace and the
    attached storage will stay in sync. One or the other might contain
    Atoms that the other does not.  It is up to you, the user, to avoid
    this inconsistncy when performing racey inserts/deletes.

    Examples.

    Delete a single Atom:
       (cog-set-value!
           (RocksStorage \"rocks:///tmp/my-rocks-db\")
           (*-delete-*)
           (Concept \"foo\"))

    Delete a multiple Atoms:
       (cog-set-value!
           (RocksStorage \"rocks:///tmp/my-rocks-db\")
           (*-delete-*)
           (ListValue (Concept \"foo\") (Concept \"bar\")))

    Specify the AtomSpace from which to delete. This is useful
    when working with frames, where the deleting may actually
    result in the hiding of an Atom:
       (cog-set-value!
           (RocksStorage \"rocks:///tmp/my-rocks-db\")
           (*-delete-*)
           (ListValue (cog-atomspace) (Concept \"foo\")))

    See also:
       *-delete-recursive-* -- Delete Atoms and any links that hold them.
       *-delete-frame-* -- Delete all the Atoms in the frame.
"
	(PredicateNode "*-delete-*")
)

(define-public (*-delete-recursive-*)
"
  (PredicateNode \"*-delete-recursive-*\") message.

    Remove one or more Atoms from storage, including any links that
    might be holding the Atoms.

    Use *-delete-* to get a non-recusive removal: the delete will fail
    if the Atoms appear in any links.

    Examples.

    Recursive delete a single Atom:
       (cog-set-value!
           (RocksStorage \"rocks:///tmp/my-rocks-db\")
           (*-delete-recursive-*)
           (Concept \"foo\"))

    Delete a multiple Atoms:
       (cog-set-value!
           (RocksStorage \"rocks:///tmp/my-rocks-db\")
           (*-delete-recurive-*)
           (ListValue (Concept \"foo\") (Concept \"bar\")))

    Specify the AtomSpace from which to delete. This is useful
    when working with frames, where the deleting may actually
    result in the hiding of an Atom:
       (cog-set-value!
           (RocksStorage \"rocks:///tmp/my-rocks-db\")
           (*-delete-recurive-*)
           (ListValue (cog-atomspace) (Concept \"foo\")))

    See also:
       *-delete-* -- Delete Atoms non-recursively.
       *-delete-frame-* -- Delete all the Atoms in the frame.
"
	(PredicateNode "*-delete-recursive-*")
)

(define*-public (cog-delete! ATOM #:optional (STORAGE (cog-storage-node)))
"
 cog-delete! ATOM [STORAGE]
    Remove the indicated ATOM, but only if it has no incoming links.
    If it has incoming links, the remove fails.  If storage is attached,
    the ATOM is also removed from the storage.

    Returns #t if the atom was removed, else returns #f if not removed.

    Use cog-extract! to remove from the AtomSpace only, leaving storage
    unaffected.

    Use cog-delete-recursive! to force removal of this atom, together
    with any links that might be holding this atom.

    If the optional STORAGE argument is provided, then it will be
    removed from that StorageNode; otherwise it will be removed from
    the current StorageNode attached to this thread.

    A word of caution about multi-threaded operation: if one thread is
    adding atoms, while another thread is removing the *same* atoms at
    the same time, then these two threads will race. As a result of
    this racing, there is no gauranteee that the AtomSpace and the
    attached storage will stay in sync. One or the other might contain
    Atoms that the other does not.  It is up to you, the user, to avoid
    this inconsistncy when performing racey inserts/deletes.

    Example usage:
       (cog-delete! (Concept \"foo\"))
       (cog-delete! (Concept \"foo\")
              (RocksStorage \"rocks:///tmp/my-rocks-db\"))
       (cog-delete! (Concept \"foo\")
              (PostgresStorage \"postgres://USER:PASSWORD@HOST/DBNAME\"))
       (cog-delete! (Concept \"foo\")
              (CogStorage \"cog://cogserver.example.com\"))

    See also:
       delete-frame! -- Delete all the Atoms in the frame.
"
	(if (< 0 (cog-incoming-size ATOM))
		#f
		(begin
			(direct-setvalue! STORAGE (*-delete-*)
				(LinkValue (cog-atomspace) ATOM)) #t))
)

(define*-public (cog-delete-recursive! ATOM #:optional (STORAGE (cog-storage-node)))
"
 cog-delete-recursive! ATOM [STORAGE]
    Remove the indicated ATOM, and all atoms that point at it.
    If storage is attached, the ATOM is also removed from storage.

    Return #t on success, else return #f if not removed.

    If the optional STORAGE argument is provided, then it will be
    removed from that StorageNode; otherwise it will be removed from
    the current StorageNode attached to this thread.

    A word of caution about multi-threaded operation: if one thread is
    adding atoms, while another thread is removing the *same* atoms at
    the same time, then these two threads will race. As a result of
    this racing, there is no gauranteee that the AtomSpace and the
    attached storage will stay in sync. One or the other might contain
    Atoms that the other does not.  It is up to you, the user, to avoid
    this inconsistncy when performing racey inserts/deletes.

    See also:
       cog-delete! -- Delete an atom from storage, w/o recursion.
       cog-extract-recursive! -- Remove an atom form the AtomSpace only.
       delete-frame! -- Delete all the Atoms in the frame.
"
	(direct-setvalue! STORAGE (*-delete-recursive-*)
		(LinkValue (cog-atomspace) ATOM))
)

; --------------------------------------------------------------------
; --------------------------------------------------------------------
; --------------------------------------------------------------------
; Messages

(define-public (*-erase-*)
"
 (Predicate \"*-erase-*\") message.

    Erase the entire contents of storage.  Use with caution to avoid
    massive data loss.

    Usage:
       (cog-set-value! (StorageNode ...) (*-erase-*) (VoidValue))
"
	(PredicateNode "*-erase-*")
)

(define*-public (cog-erase! #:optional (STORAGE (cog-storage-node)))
"
 cog-erase! [STORAGE]

    Convenience wrapper for the (*-erase-*) message.

    Same as
       (cog-set-value! STORAGE (*-erase-*) (VoidValue))

    If the optional STORAGE argument is provided, then the erase will
    be applied to it. It must be a StorageNode.
"
	(direct-setvalue! STORAGE (*-erase-*) (VoidValue))
)

(define-public (*-load-frames-*)
"
  (Predicate \"*-load-frames-*\") message.

  Load the DAG of AtomSpaces from storage.

    This will load the DAG of AtomSpaces held in the storage server to
    be created. This will only create the AtomSpaces; it will NOT
    populate them with Atoms. These have to be either fetched in bulk,
    or individually, using the usual methods.

    Returns the DAG

    Usage:
       (cog-value (StorageNode ...) (*-load-frames-*))

    See also:
    *-store-frames-* -- store the DAG of AtomSpaces to storage.
    *-fetch-atom-* -- fetch an individual ATOM, and all Values on it.
    *-fetch-query-* -- get all Atoms for a given QUERY.
    *-load-referrers-* -- get every graph that contains ATOM.
    *-load-atoms-of-type-* -- load only atoms of type TYPE.
    *-load-atomspace-* -- load the entire contents of storage.
"
	(PredicateNode "*-load-frames-*")
)

(define*-public (load-frames #:optional (STORAGE (cog-storage-node)))
"
 load-frames [STORAGE] - load the DAG of AtomSpaces from storage.

    Convenience wrapper for the (*-load-frames-*) message.
    Same as
       (cog-value STORAGE (*-load-frames-*))

    If the optional STORAGE argument is provided, then it will be
    used as the source of the load. It must be a StorageNode.
"
	(cog-value->list (cog-value STORAGE (*-load-frames-*)))
)

(define-public (*-store-frames-*)
"
  (Predicate \"*-store-frames-*\") message.

  Store the DAG of AtomSpaces to storage.

    This will store the DAG of AtomSpaces to the storage server.  This
    will only store the DAG of the AtomSpaces; it will NOT store their
    contents.  These have to be either stored in bulk, or individually,
    using the usual messages.

    Usage:
       (cog-set-value! (StorageNode ...) (*-store-frames-*) ATOMSPACE)

    See also:
    *-load-frames-* -- load the DAG of AtomSpaces from storage.
    *-store-atomspace-* -- store the entire contents of an AtomSpace.
    *-store-atom-* -- store an individual Atom.
"
	(PredicateNode "*-store-frames-*")
)

(define*-public (store-frames ATOMSPACE #:optional (STORAGE (cog-storage-node)))
"
 store-frames ATOMSPACE [STORAGE] - store the DAG of AtomSpaces to storage.

    Convenience wrapper for the (*-store-frames-*) message.
    Same as
       (cog-set-value! STORAGE (*-store-frames-*) ATOMSPACE)

    If the optional STORAGE argument is provided, then it will be
    used as the target of the store. It must be a StorageNode.
"
	(direct-setvalue! STORAGE (*-store-frames-*) ATOMSPACE)
)

(define-public (*-delete-frame-*)
"
 (Predicate \"*-delete-frame-*\") message.

    Delete the contents of the AtomSpace.

    This will delete all of the Atoms in the AtomSpace, as well as the
    associated frame, so that it no longer appears in the frame DAG.
    Note that this will also delete any Atoms that have been marked
    hidden, and thus might cause the corresponding Atoms in lower frames
    to become visible.

    Caveats:
    The StorageNode must be open for writing.
    At this time, only the top-most frame can be deleted.
    The frame is deleted in storage only; the atoms remain in RAM
    until all references to that AtomSpace are gone. Use
    `cog-atomspace-clear` to also remove these atoms.

    Usage:
       (cog-set-value! (StorageNode ...)
          (*-delete-frame-*) (cog-atomspace))

    See also:
       *-load-frames-* -- load the DAG of AtomSpaces from storage.
       *-store-frames-* -- store the DAG of AtomSpaces to storage.
       *-clear-* -- extract all atoms in a frame.
"
	(PredicateNode "*-delete-frame-*")
)

(define*-public (delete-frame! ATOMSPACE #:optional (STORAGE (cog-storage-node)))
"
 delete-frame! ATOMSPACE [STORAGE] - delete the contents of the AtomSpace.

    Convenience wrapper for the (*-delete-frame-*) message.
    Same as
       (cog-set-value! STORAGE (*-delete-frame-*) ATOMSPACE)
"
	(direct-setvalue! STORAGE (*-delete-frame-*) ATOMSPACE)
)

(define-public (*-barrier-*)
"
  (PredicateNode \"*-narrier-*\") message

    Block (do not return to the caller) until the storage write queues
    are empty. Just because the atomspace write queues are empty, it
    does not mean that the data was actually written to disk. It merely
    means that the atomspace, as a client of the storage server, has
    given them to the server.

    Usage:
       (cog-set-value! (StorageNode ...) (*-barrier-*) (cog-atomspace))
"
	(PredicateNode "*-barrier-*")
)

(define*-public (barrier #:optional (STORAGE (cog-storage-node)))
"
 barrier [STORAGE]

    Convenience wrapper for the (*-barrier-*) message.
    Same as
       (cog-set-value! STORAGE (*-barrier-*) (cog-atomspace))

    If the optional STORAGE argument is provided, then the barrier will
    be applied to it. It must be a StorageNode.
"
	(direct-setvalue! STORAGE (*-barrier-*) (cog-atomspace))
)

(define-public (*-monitor-*)
"
  (PredicateNode \"*-monitor-*\") message

    Return a StringValue containing storage performance monitoring
    and debugging information. To display the string in a properly
    formatted fashion, say `(display (monitor-storage))`.

    Note that some StorageNodes might do significant computations
    before returning a report, and thus may appear to hang. Patience!

  Usage:
    (cog-value STORAGE (*-monitor-*))
    (display (cog-value-ref STORAGE (*-monitor-*) 0))

    See also:
       `*-open-*` to open a connection.
       `*-close-*` to close a connection.
       `*-connected?-*` to obtain the connection status.
       `cog-storage-node` to obtain the current connection.
"
	(PredicateNode "*-monitor-*")
)

(define*-public (monitor-storage #:optional (STORAGE (cog-storage-node)))
"
 monitor-storage [STORAGE]

    Convenience wrapper for the (*-monitor-*) message.
    Same as
       (cog-value STORAGE (*-monitor-*))
"
   (cog-value-ref STORAGE (*-monitor-*) 0)
)

(define-public (*-proxy-parts-*)
"
  (PredicateNode \"*-proxy-parts-*\") message

  Specify the component parts (e.g. mirrors) of a proxy.

    Example:
       (define rsn (RocksStorageNode \"rocks:///tmp/foo.rdb\"))
       (define pxy (WriteThruProxy \"any name will do\"))
       (cog-set-value! pxy (*-proxy-parts-*) rsn))

    See also:
       `*-proxy-close-*` to halt proxying.
       `*-set-proxy-*` to declare the remote proxy.
       `*-decay-const-*` to specify a time interval to buffering proxies
"
	(PredicateNode "*-proxy-parts-*")
)

(define-public (*-decay-const-*)
"
  (PredicateNode \"*-decay-const-*\") message

  Specify a time interval to a buffering proxy.
  Conventionaly, the number is undertood to be seconds.

    Example:
       (define pxy (WriteBufferProxy \"buffy slayer\"))
       (cog-set-value! pxy (*-decay-const-*) (NumberNode 42))

    See also:
       `*-proxy-parts-*` to specify the components making up a proxy.
"
	(PredicateNode "*-decay-const-*")
)

(define-public (*-proxy-open-*)
"
  (PredicateNode \"*-proxy-open-*\") message

  Start proxying at the remote end.

    Pass a `*-open-*` message to a proxy at the remote end of a network
    connection. This works only for StorageNodes that have a remote end;
    that is, for StorageNodes that can connect to other AtomSpaces.
    Examples of these include CogStorageNode and CogSimpleStorageNode,
    which can exchange Atoms and Values with a remote AtomSpace.

    Proxies are StorageNodes that support the StorageNode API, and then
    satisfy API requests by passing them on to other StorageNodes. For
    example, the ReadThruProxy passes on all API Atom and Value fetch
    requests to other StorageNodes.

    A typical setup is to have a CogServer pass on network I/O to a
    disk-based StorageNode (e.g. the RocksStorageNode.) The users of
    the CogServer (i.e. the users of a CogStorageNode) must first tell
    the remote end to open the proxy, before this pass-thru can happen.

    Since ProxyNodes are just like ordinary StorageNodes, they must be
    opened before they can be used. Since they are in a remote AtomSpace,
    there is no way to access them directly; the `*-proxy-open-*`
    message will perform that open.

    Example:
       (define rsn (RocksStorageNode \"rocks:///tmp/foo.rdb\"))
       (define pxy (WriteThruProxy \"any name will do\"))
       (cog-set-value! pxy (*-proxy-parts-*) rsn))

       (define csn (CogStorageNode \"cog://example.com:17001\"))
       (cog-open csn)
       (cog-set-value! csn (*-set-proxy-*) pxy)
       (cog-set-value! csn (*-proxy-open-*) (VoidValue))
       (store-atom (Concept \"foo\"))

    The above example will cause the Atom `(Concept \"foo\"))` to be
    sent to the CogServer at `example.com`, which will then write it
    into the RocksDB database.  Note that proxies allow arbitrarily
    complex dataflow networks to be defined.

    See also:
       `*-proxy-close-*` to halt proxying.
       `*-set-proxy-*` to declare the remote proxy.
"
	(PredicateNode "*-proxy-open-*")
)

(define*-public (cog-proxy-open #:optional (STORAGE (cog-storage-node)))
"
 cog-proxy-open [STORAGE] - Start proxying at the remote end.

    Convenience wrapper for the (*-proxy-open-*) message.
    Same as
       (cog-set-value! STORAGE (*-proxy-open-*) (VoidValue))
"
	(direct-setvalue! STORAGE (*-proxy-open-*) (VoidValue))
)

(define-public (*-proxy-close-*)
"
  (PredicateNode \"*-proxy-close-*\") message

    Stop proxying at the remote end.  This halts proxying
    previously started with the `*-proxy-open=*` message.

    Usage:
       (cog-set-value! (StorageNode ...) (*-proxy-close-*) (VoidValue))

    See also:
       `*-proxy-open-*` to start proxying.
       `*-set-proxy-*` to declare the remote proxy.
"
	(PredicateNode "*-proxy-close-*")
)

(define*-public (cog-proxy-close #:optional (STORAGE (cog-storage-node)))
"
 cog-proxy-close [STORAGE] - Stop proxying at the remote end.

    Convenience wrapper for the (*-proxy-close-*) message.
    Same as
       (cog-set-value! STORAGE (*-proxy-close-*) (VoidValue))
"
	(direct-setvalue! STORAGE (*-proxy-close-*) (VoidValue))
)

(define-public (*-set-proxy-*)
"
  (PredicateNode \"*-set-proxy-*\") message

    Declares a ProxyNode to the remote end of a network connection.

    ProxyNodes are StorageNodes that support the StorageNode API, and
    then satisfy API requests by passing them on to other StorageNodes.
    For example, the ReadThruProxy passes on all API Atom and Value
    fetch requests to other StorageNodes.

    Since ProxyNodes are just like ordinary StorageNodes, they must be
    opened before they can be used. Since they are in a remote AtomSpace,
    there is no way to access them directly; the `cog-proxy-open` command
    will perform that open.

    Usage:
       (cog-set-value! (StorageNode ...) (*-set-proxy-*) (ProxyNode ...))

    See also:
       `*-proxy-open-*` to start proxying.
       `*-proxy-close-*` to stop proxying.
"
	(PredicateNode "*-set-proxy-*")
)

(define*-public (cog-set-proxy! PROXY #:optional (STORAGE (cog-storage-node)))
"
 cog-set-proxy! PROXY [STORAGE] - Declare a proxy to the remote end.

    Convenience wrapper for the *-set-proxy-* message.
    Same as
       (cog-set-value! STORAGE (*-set-proxy-*) PROXY)
"
	(direct-setvalue! STORAGE (*-set-proxy-*) PROXY)
)

; --------------------------------------------------------------------
