;
; OpenCog CSV Table Reader module
;

(define-module (opencog csv-table))

(use-modules (opencog))
(use-modules (opencog as-config))
(load-extension
	(string-append opencog-ext-path-csv-table "libcsv-table")
	"opencog_csv_table_init")

(export load-table)

(set-procedure-property! load-table 'documentation
"
 load-table ATOM FILE -- Load CSV/TSV table from FILE.

    Throws error if FILE does not exist.
    More documentation TBD
")

; --------------------------------------------------------------------
