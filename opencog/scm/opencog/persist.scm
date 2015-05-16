;
; OpenCog Persistance module
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
