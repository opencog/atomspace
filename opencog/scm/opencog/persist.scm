;
; OpenCog Persistance module
; Copyright (C) 2009 Linas Vepstas <linasvepstas@gmail.com>
;

(define-module (opencog persist))

(use-modules (opencog))
(use-modules (opencog as-config))
(load-extension (string-append opencog-ext-path-persist "libpersist") "opencog_persist_init")

; This avoids complaints, when the docs are set, below.
(export
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

(set-procedure-property! fetch-atom 'documentation
"
 fetch-atom ATOM

    Fetch all of the Values on the indicated ATOM from storage.
    This updates (clobbers) all of the values in the atomspace,
    and replaces them with the ones fetched from storage.

    See also:
       `fetch-value` to get only one Value.
       `store-atom` to store all Values.
")

(set-procedure-property! fetch-value 'documentation
"
 fetch-value ATOM KEY

    Fetch from storage the Value located at KEY on ATOM.
    This updates (clobbers) any current Value stored at KEY,
    replacing it with the one fetched from storage.

    See also:
       `fetch-atom` to get all Values.
       `store-value` to store only one Value.
")

(set-procedure-property! fetch-incoming-set 'documentation
"
 fetch-incoming-set ATOM

    Fetch the incoming set of the ATOM from storage. The fetch is
    NOT recursive.  See `load-referers` for a recursive fetch.

    See also `fetch-incoming-by-type`.
")

(set-procedure-property! fetch-incoming-by-type 'documentation
"
 fetch-incoming-by-type ATOM TYPE

    Fetch those links of the incoming set of ATOM that are of type TYPE.
    This is a more limited fetch than the one done by `fetch-incoming-set`
    and can be useful when the incoming set is large.
")

(set-procedure-property! store-atom 'documentation
"
 store-atom ATOM

    Store indicated ATOM, and all of its associated keys and values,
    to storage. This updates (clobbers) the values previously held
    in storage, replacing them by the values in the atomspace.

    See also:
       `store-value` to store just one Value.
       `fetch-atom` to fetch all Values on an Atom.
")

(set-procedure-property! store-value 'documentation
"
 store-value ATOM KEY

    Store the Value located at KEY on ATOM. This updates (clobbers)
    the Value previously held in storage, replacing it by the Value
    in the atomspace.

    See also:
       `store-atom` to store all values on an Atom.
       `fetch-value` to fetch just one Value.
")

(set-procedure-property! load-atoms-of-type 'documentation
"
 load-atoms-of-type TYPE

    Fetch atoms of the given TYPE from storage. This fetches the
    atoms, and all the associated values attached to them.
")

(set-procedure-property! barrier 'documentation
"
 barrier

    Block (do not return to the caller) until the storage write queues
    are empty. Just because the atomspace write queues are empty, it
    does not mean that the data was actually written to disk. It merely
    means that the atomspace, as a client of the storage server, has
    given them to the server.
")

(set-procedure-property! load-atomspace 'documentation
"
 load-atomspace - load all atoms from storage.

    This will cause ALL of the atoms in the open storage server to be
    loaded into the current AtomSpace. This can be a very time-consuming
    operation.  In normal operation, it is rarely necessary to load all
    atoms; there are several ways to fetch subsets of atoms, or even one
    at a time, when needed.

    See also:
    fetch-atom ATOM -- fetch an individual ATOM, and all Values on it.
    fetch-incoming-set ATOM -- fetch the entire incoming set of ATOM.
    fetch-incoming-by-type ATOM TYPE -- get a sbset of the incoming set.
    load-referers ATOM -- get every graph that contains ATOM
    load-atoms-of-type TYPE -- load only atoms of type TYPE
")

(set-procedure-property! store-atomspace 'documentation
"
 store-atomspace - Store all atoms in the AtomSpace to storage.

    This will dump the ENTIRE contents of the current AtomSpace to the
    the currently-open storage.  Depending on the size of the AtomSpace,
    this may take a lot of time.  During normal operation, a bulk-save
    is rarely required, as individual atoms can always be stored, one
    at a time.

    See also:
    store-atom ATOM -- store one ATOM and all of the values on it.
    store-referers ATOM -- store all graphs that contain ATOM
")

;
; --------------------------------------------------------------------
(define-public (store-referers ATOM)
"
 store-referers ATOM -- Store all hypergraphs that contain ATOM

   This stores all hypergraphs that the ATOM participates in.
   It does this by recursively exploring the incoming set of the atom.

   See also `load-referers`.
"
	(define (do-store atom)
		(let ((iset (cog-incoming-set atom)))
			(if (null? iset)
				(store-atom atom)
				(for-each do-store iset)
			)
		)
	)
	(do-store ATOM)
)

; --------------------------------------------------------------------
(define-public (load-referers atom)
"
 load-referers ATOM -- Load (from storage) all graphs that contain ATOM.

   This loads all hypergraphs that the given ATOM participates in.
   It does this by recursively exploring the incoming set of the atom.

   See also `store-referers`.
"
	(if (not (null? atom))
		; The fetch-incoming-set function for this is defined to perform
		; a recursive fetch.
		; We do an extra recursion here, in case we were passed a list.
		(begin
			(if (pair? atom)
				(for-each load-referers atom)
				(fetch-incoming-set atom)
			)
			(for-each load-referers (cog-incoming-set atom))
		)
	)
)

; --------------------------------------------------------------------
