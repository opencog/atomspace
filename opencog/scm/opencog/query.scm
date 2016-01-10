;
; OpenCog Pattern matcher module
;

(define-module (opencog query))

; The atomspace libraries are located in /usr/local/lib/opencog
(setenv "LTDL_LIBRARY_PATH"
	(if (getenv "LTDL_LIBRARY_PATH")
		(string-append (getenv "LTDL_LIBRARY_PATH") ":/usr/local/lib/opencog")
		"/usr/local/lib/opencog"))

; This is also loaded by (opencog exec) We need it here,
; else we get undefined symbols in libquery.
(load-extension "libexecution" "opencog_exec_init")

(load-extension "libquery" "opencog_query_init")

(set-procedure-property! cog-bind 'documentation
"
 cog-bind handle
    Run pattern matcher on handle.  handle must be a BindLink.
    Uses crisp (non-probabilistic) logic during the evaluation
    of evaluatable terms.
")

(set-procedure-property! cog-bind-single 'documentation
"
 cog-bind-single handle
    Run pattern matcher on handle.  handle must be a BindLink.
    The search is terminated after the first match is found.
")

(set-procedure-property! cog-bind-af 'documentation
"
 cog-bind-af handle
    Run pattern matcher on handle.  handle must be a BindLink.
    A special-purpose pattern matcher used by the URE.
")

(set-procedure-property! cog-satisfy 'documentation
"
 cog-satisfy handle
    Run pattern matcher on handle.  handle must be a SatisfactionLink.
    Return a TV. Only satisfaction is performed, no implication.
")
