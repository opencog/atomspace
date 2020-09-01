;
; OpenCog Persistance module
; Copyright (C) 2009 Linas Vepstas <linasvepstas@gmail.com>
;

(define-module (opencog persist))

(use-modules (ice-9 optargs)) ; for define*-public

(use-modules (opencog))
(use-modules (opencog as-config))
(load-extension (string-append opencog-ext-path-persist "libpersist") "opencog_persist_init")

(include-from-path "opencog/persist/types/storage_types.scm")

; This avoids complaints, when the docs are set, below.
(export
	cog-open
	cog-close
	fetch-atom
	fetch-value
	fetch-incoming-set
	fetch-incoming-by-type
	fetch-query
	store-atom
	store-value
	load-atoms-of-type
	barrier
	load-atomspace
	store-atomspace)

;; -----------------------------------------------------
;;
(set-procedure-property! cog-open 'documentation
"
 cog-open STORAGE-ATOM

    Open a connection to the indicated STORAGE-ATOM. An open connection
    allows Atoms to be sent/received along this connection.

    Examples:
       (cog-open (PostgresStorage \"postgres:///example-db?user=foo&password=bar\"))
       (cog-open (RocksStorage \"rocks:///tmp/my-rocks-db\"))
       (cog-open (CogserverStorage \"cog://localhost:17001\"))

    See also:
       `cog-close` to close a connection.
")

(set-procedure-property! cog-close 'documentation
"
 cog-close STORAGE-ATOM

    Close an open connection to the indicated STORAGE-ATOM. Closing the
    connection disables further communication on the connection. The
    STORAGE-ATOM indicates which connection to close.

    Examples:
       (cog-close (PostgresStorage \"postgres:///example-db?user=foo&password=bar\"))
       (cog-close (RocksStorage \"rocks:///tmp/my-rocks-db\"))
       (cog-close (CogserverStorage \"cog://localhost:17001\"))

    See also:
       `cog-open` to open a connection.
")

(define*-public (fetch-atom ATOM #:optional (STORAGE #f))
"
 fetch-atom ATOM [STORAGE]

    Fetch all of the Values on the indicated ATOM from storage.
    This updates (clobbers) all of the values in the atomspace,
    and replaces them with the ones fetched from storage.

    If the optional STORAGE argument is provided, then it will be
    used as the source of the fetch. It must be a StorageNode.

    See also:
       `fetch-value` to get only one Value.
       `store-atom` to store all Values.
"
	(if STORAGE (sn-fetch-atom ATOM STORAGE) (dflt-fetch-atom ATOM))
)

(define*-public (fetch-value ATOM KEY #:optional (STORAGE #f))
"
 fetch-value ATOM KEY [STORAGE]

    Fetch from storage the Value located at KEY on ATOM.
    This updates (clobbers) any current Value stored at KEY,
    replacing it with the one fetched from storage.

    If the optional STORAGE argument is provided, then it will be
    used as the source of the fetch. It must be a StorageNode.

    See also:
       `fetch-atom` to get all Values.
       `store-value` to store only one Value.
"
	(if STORAGE (sn-fetch-value ATOM KEY STORAGE) (dflt-fetch-value ATOM KEY))
)

(define*-public (fetch-incoming-set ATOM #:optional (STORAGE #f))
"
 fetch-incoming-set ATOM [STORAGE]

    Fetch the incoming set of the ATOM from storage. The fetch is
    NOT recursive.  See `load-referers` for a recursive fetch.

    If the optional STORAGE argument is provided, then it will be
    used as the source of the fetch. It must be a StorageNode.

    See also:
      `load-referers` to get every graph that contains an Atom.
      `fetch-incoming-by-type` to fetch a subset of a given type.
      `fetch-query` to fetch a query-defined collection of Atoms.
"
	(if STORAGE (sn-fetch-incoming-set ATOM STORAGE)
		(dflt-fetch-incoming-set ATOM))
)

(define*-public (fetch-incoming-by-type ATOM TYPE #:optional (STORAGE #f))
"
 fetch-incoming-by-type ATOM TYPE [STORAGE]

    Fetch those links of the incoming set of ATOM that are of type TYPE.
    This is a more limited fetch than the one done by `fetch-incoming-set`
    and can be useful when the incoming set is large.

    If the optional STORAGE argument is provided, then it will be
    used as the source of the fetch. It must be a StorageNode.

    See also:
      `load-referers` to get every graph that contains an Atom.
      `fetch-incoming-set` to fetch all of the incoming set.
      `fetch-query` to fetch a query-defined collection of Atoms.
"
	(if STORAGE (sn-fetch-incoming-by-type TYPE STORAGE)
		(dflt-fetch-incoming-by-type ATOM TYPE))
)

(define*-public (store-atom ATOM #:optional (STORAGE #f))
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
	(if STORAGE (sn-store-atom ATOM STORAGE) (dflt-store-atom ATOM))
)

(define*-public (store-value ATOM KEY #:optional (STORAGE #f))
"
 store-value ATOM KEY [STORAGE]

    Store the Value located at KEY on ATOM. This updates (clobbers)
    the Value previously held in storage, replacing it by the Value
    in the atomspace.

    If the optional STORAGE argument is provided, then it will be
    used as the target of the store. It must be a StorageNode.

    See also:
       `store-atom` to store all values on an Atom.
       `fetch-value` to fetch just one Value.
"
	(if STORAGE (sn-store-value ATOM KEY STORAGE) (dflt-store-value ATOM KEY))
)

(define*-public (load-atoms-of-type TYPE #:optional (STORAGE #f))
"
 load-atoms-of-type TYPE [STORAGE]

    Fetch atoms of the given TYPE from storage. This fetches the
    atoms, and all the associated values attached to them.

    If the optional STORAGE argument is provided, then it will be
    used as the source of the load. It must be a StorageNode.
"
	(if STORAGE (sn-load-atoms-of-type TYPE STORAGE)
		(dflt-load-atoms-of-type TYPE))
)

(define*-public (barrier #:optional (STORAGE #f))
"
 barrier [STORAGE]

    Block (do not return to the caller) until the storage write queues
    are empty. Just because the atomspace write queues are empty, it
    does not mean that the data was actually written to disk. It merely
    means that the atomspace, as a client of the storage server, has
    given them to the server.

    If the optional STORAGE argument is provided, then the barrier will
    be applied to it. It must be a StorageNode.
"
	(if STORAGE (sn-barrier STORAGE) (dflt-barrier))
)

(define*-public (load-atomspace #:optional (STORAGE #f))
"
 load-atomspace [STORAGE] - load all atoms from storage.

    This will cause ALL of the atoms in the open storage server to be
    loaded into the current AtomSpace. This can be a very time-consuming
    operation.  In normal operation, it is rarely necessary to load all
    atoms; there are several ways to fetch subsets of atoms, or even one
    at a time, when needed.

    If the optional STORAGE argument is provided, then it will be
    used as the source of the load. It must be a StorageNode.

    See also:
    fetch-atom ATOM -- fetch an individual ATOM, and all Values on it.
    fetch-incoming-set ATOM -- fetch the entire incoming set of ATOM.
    fetch-incoming-by-type ATOM TYPE -- get a subset of the incoming set.
    fetch-query QUERY -- get all Atoms for a given QUERY.
    load-referers ATOM -- get every graph that contains ATOM
    load-atoms-of-type TYPE -- load only atoms of type TYPE
"
	(if STORAGE (sn-load-atomspace STORAGE) (dflt-load-atomspace))
)

(define*-public (store-atomspace #:optional (STORAGE #f))
"
 store-atomspace [STORAGE] - Store all atoms in the AtomSpace to storage.

    This will dump the ENTIRE contents of the current AtomSpace to the
    the currently-open storage.  Depending on the size of the AtomSpace,
    this may take a lot of time.  During normal operation, a bulk-save
    is rarely required, as individual atoms can always be stored, one
    at a time.

    If the optional STORAGE argument is provided, then it will be
    used as the target of the store. It must be a StorageNode.

    See also:
    store-atom ATOM -- store one ATOM and all of the values on it.
    store-referers ATOM -- store all graphs that contain ATOM
"
	(if STORAGE (sn-store-atomspace STORAGE) (dflt-store-atomspace))
)

;
; --------------------------------------------------------------------
(define*-public (fetch-query QUERY KEY
	#:optional (METADATA '()) (FRESH #f) (STORAGE #f))
"
 fetch-query QUERY KEY [METADATA [FRESH]] [STORAGE]

   Perform the QUERY at the storage server, and load the results into
   the AtomSpace. The results will be returned directly and also cached
   at KEY. The QUERY must be either a JoinLink, MeetLink or QueryLink.

   This can be thought of as a generalization of `load-referers`,
   `fetch-incoming-set` and `fetch-incoming-by-type`. Thus, the
   simplest JoinLink is effectively the same thing as `load-referers`,
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
     `load-referers` to fetch all graphs containing an Atom.
"
	(if (nil? METADATA)
		(dflt-fetch-query-2args QUERY KEY)
		(if (cog-subtype? METADATA 'StorageNode)
			(sn-fetch-query-2args QUERY KEY METADATA)
			(if STORAGE
				(sn-fetch-query-4args QUERY KEY METADATA FRESH STORAGE)
				(dflt-fetch-query-4args QUERY KEY METADATA FRESH))))
)

; --------------------------------------------------------------------
(define*-public (store-referers ATOM #:optional (STORAGE #f))
"
 store-referers ATOM [STORAGE] -- Store all hypergraphs that contain ATOM

    This stores all hypergraphs that the ATOM participates in.
    It does this by recursively exploring the incoming set of the atom.

    If the optional STORAGE argument is provided, then it will be
    used as the target of the store. It must be a StorageNode.

    See also `load-referers`.
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
(define*-public (load-referers ATOM #:optional (STORAGE #f))
"
 load-referers ATOM [STORAGE] -- Load all graphs that contain ATOM.

   This loads all hypergraphs that the given ATOM participates in.
   It does this by recursively exploring the incoming set of the atom.

   If the optional STORAGE argument is provided, then it will be
   used as the source of the load. It must be a StorageNode.

   See also:
     `fetch-incoming-set` to fetch only the first level above an Atom.
     `fetch-query` to perform a generalized query for holders of an Atom.
     `store-referers` to store all referers.
"
	(if (not (null? ATOM))
		; The fetch-incoming-set function for this is defined to perform
		; a recursive fetch.
		; We do an extra recursion here, in case we were passed a list.
		(begin
			(if (pair? ATOM)
				(for-each load-referers ATOM STORAGE)
				(fetch-incoming-set ATOM STORAGE)
			)
			(for-each
				(lambda (atm) (load-referers atm STORAGE))
				(cog-incoming-set ATOM))
		)
	)
)

; --------------------------------------------------------------------
