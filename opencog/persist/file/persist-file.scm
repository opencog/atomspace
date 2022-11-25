;
; OpenCog File Persistence module
;

(define-module (opencog persist-file))

(use-modules (opencog))
(use-modules (opencog persist))
(use-modules (opencog as-config))
(load-extension
	(string-append opencog-ext-path-persist-file "libpersist-file")
	"opencog_persist_file_init")

(export load-file)

(set-procedure-property! load-file 'documentation
"
 load-file FILE -- Load atomese from FILE.

    Throws error if FILE does not exist.
")

; --------------------------------------------------------------------
