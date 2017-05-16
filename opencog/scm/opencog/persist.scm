;
; OpenCog Persistance module
; Copyright (C) 2009 Linas Vepstas <linasvepstas@gmail.com>
;

(define-module (opencog persist))

(load-extension "libpersist" "opencog_persist_init")

;; -----------------------------------------------------
;;

(set-procedure-property! fetch-atom 'documentation
"
 fetch-atom ATOM
    Fetch indicated ATOM from SQL/persistent storage.
")

(set-procedure-property! fetch-incoming-set 'documentation
"
 fetch-incoming-set ATOM
    Fetch the incoming set of the ATOM from SQL storage. The fetch is
    NOT recursive.  See load-referers for a recursive fetch.

    See also fetch-incoming-by-type.
")

(set-procedure-property! fetch-incoming-by-type 'documentation
"
 fetch-incoming-by-type ATOM TYPE
    Fetch those links of the incoming set of ATOM that are of type TYPE.
    This is a more limited fetch than the one done by fetch-incoming-set
    and can be useful when the incoming set is large.
")

(set-procedure-property! store-atom 'documentation
"
 store-atom ATOM
    Store indicated ATOM to SQL/persistent storage.
")

(set-procedure-property! load-atoms-of-type 'documentation
"
 load-atoms-of-type TYPE
    Fetch atoms of the given TYPE from SQL/persistent storage.
")

(set-procedure-property! barrier 'documentation
"
 barrier
    Block until the SQL Atom write queues are empty.
")

;
; --------------------------------------------------------------------
(define-public (store-referers ATOM)
"
 store-referers ATOM -- Store to SQL all hypergraphs that contain ATOM

 This stores all hypergraphs that the ATOM participates in.
 It does this by recursively exploring the incoming set of the atom.

 See also load-referers.
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

 See also store-referers.
"
	(if (not (null? atom))
		; The fetch-incoming-set function for this is defined to perform
		; a recursive fetch.
		; We do an extra recursion here, in case we were passed a list.
		(if (pair? atom)
			(for-each load-referers atom)
			(fetch-incoming-set atom)
		)
	)
)

; --------------------------------------------------------------------
