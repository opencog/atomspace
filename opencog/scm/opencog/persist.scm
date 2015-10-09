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
 fetch-atom handle
    Fetch indicated atom from SQL/persistent storage.
")

(set-procedure-property! fetch-incoming-set 'documentation
"
 fetch-incoming-set
    Fetch the incoming set of the atom from SQL storage. The fetch is
    recursive.
")

(set-procedure-property! store-atom 'documentation
"
 store-atom handle
    Store indicated atom to SQL/persistent storage.
")

(set-procedure-property! load-atoms-of-type 'documentation
"
 load-atoms-of-type type
    Fetch atoms of the given type from SQL/persistent storage.
")

(set-procedure-property! barrier 'documentation
"
 barrier
    Block until the SQL Atom write queues are empty.
")

;
; --------------------------------------------------------------------
(define-public (store-referers atomo)
"
 store-referers -- Store to SQL all hypergraphs that contain given atom

 This stores all hypergraphs that the given atom participates in.
 It does this by recursively exploring the incoming set of the atom.
"
	(define (do-store atom)
		(let ((iset (cog-incoming-set atom)))
			(if (null? iset)
				(store-atom atom)
				(for-each do-store iset)
			)
		)
	)
	(do-store atomo)
)

; --------------------------------------------------------------------
(define-public (load-referers atom)
"
 load-referers -- Load from SQL all hypergraphs that contain given atom

 This loads all hypergraphs that the given atom participates in.
 It does this by recursively exploring the incoming set of the atom.
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
