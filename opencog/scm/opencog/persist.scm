;
; OpenCog Persistance module
; Copyright (C) 2009 Linas Vepstas <linasvepstas@gmail.com>
;

(define-module (opencog persist))

(use-modules (opencog))

(load-extension "libpersist" "opencog_persist_init")

; This avoids complaints, when the docs are set, below.
(export fetch-atom fetch-incoming-set fetch-incoming-by-type
store-atom load-atoms-of-type barrier)

;; -----------------------------------------------------
;;

(set-procedure-property! fetch-atom 'documentation
"
 fetch-atom ATOM
    Fetch all of the values on the indicated ATOM from SQL/persistent
    storage. This updates (clobbers) all of the values in the atomspace,
    and replaces them with the ones fetched from the database.
")

(set-procedure-property! fetch-incoming-set 'documentation
"
 fetch-incoming-set ATOM
    Fetch the incoming set of the ATOM from SQL storage. The fetch is
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
    Store indicated ATOM, and all of its associated keys and values, to
    SQL/persistent storage. This updates (clobbers) the values
    previously stored in the database, replacing them by the values in
    the atomspace.
")

(set-procedure-property! load-atoms-of-type 'documentation
"
 load-atoms-of-type TYPE
    Fetch atoms of the given TYPE from SQL/persistent storage. This
    fetches the atoms, and all the associated values attached to them.
")

(set-procedure-property! barrier 'documentation
"
 barrier
    Block (do not return to the caller) until the SQL Atom write queues
    are empty. Just because the atomspace write queues are empty, it
    does not mean that the data was actually written to disk. It merely
    means that the atomspace, as a client of the database, has given
    them to the database.
")

;
; --------------------------------------------------------------------
(define-public (store-referers ATOM)
"
 store-referers ATOM -- Store to SQL all hypergraphs that contain ATOM

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
 load-referers ATOM -- Load from SQL all hypergraphs that contain ATOM

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
