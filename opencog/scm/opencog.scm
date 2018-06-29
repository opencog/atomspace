;
; Main OpenCog guile module
;
; When this module is loaded from the guile prompt, it sets up all of
; the opencog infrastructure, including a default atomspace.
;
; To use, say this from the guile prompt:
; (use-modules (opencog))
;
;
; This should result in a utf8 locale being used!
; See https://github.com/opencog/opencog/issues/937
(setlocale LC_CTYPE "")
(setlocale LC_NUMERIC "C")

; libsmob won't be found unless we setenv where to find it!
; In theory, we should have installed it into one of these locations:
;    /usr/lib/guile/2.0/extensions
;    /usr/local/lib/guile/2.0/extensions
;    /usr/lib64/guile/2.0/extensions
;    /usr/local/lib64/guile/2.0/extensions
;    /usr/lib/guile/2.2/extensions
;    /usr/local/lib/guile/2.2/extensions
;    /usr/lib64/guile/2.2/extensions
;    /usr/local/lib64/guile/2.2/extensions
;
; But which one? Its a pain, so we wing it, below, and use
; LTDL_LIBRARY_PATH
;

; lib64 is used by various versions of CentOS
(define path "/usr/local/lib/opencog:/usr/local/lib64/opencog")
(setenv "LTDL_LIBRARY_PATH"
	(if (getenv "LTDL_LIBRARY_PATH")
		(string-append (getenv "LTDL_LIBRARY_PATH") ":" path)
		path))

; Work-around another common usability issue...
(add-to-load-path "/usr/local/share")
(add-to-load-path "/usr/local/share/opencog/scm")

(define-module (opencog))
(load-extension "libsmob" "opencog_guile_init")

(use-modules (system base compile))

; We need to list everything that was already exported by the shared
; library; failure to do so causes warning messages to be printed,
; because other scheme code cannot guess what names the shared lib
; actually exported.  So we list them here.
(export
	cog-arity
	cog-atomspace
	cog-get-atoms
	cog-incoming-by-type
	cog-link
	cog-new-atomspace
	cog-new-ctv
	cog-new-stv
	cog-node
	cog-outgoing-set
	cog-set-atomspace!
	cog-set-tv!
	cog-set-value!
	cog-tv
	cog-tv-count
	cog-type
	cog-value
	cog-value-ref
)

; Create a global to hold the atomspace ... to (try to) prevent guile
; GC from collecting it.  Unfortunately, there appears to be a GC bug
; in guile-2.1 that causes this to be collected, anyway.  Its as if
; guile forgets about this ... how? why? I don't get it.
;
; But wait -- it gets worse. If this module is loaded from the guile
; REPL (i.e. as "(use-modules (opencog))" ), and then the cogserver
; is started from the REPL, as "(start-cogserver "cogserver.conf"),
; then there is a good chance that this file will be loaded (again),
; directly by the SCM_PRELOAD directive in the cogserver.conf file.
; Viz, the stuff in here may end up running a second time. We want
; to avoid creating a second atomspace as a result. The below tries
; to avoid problems by simply grabbing the existing atomspace, when
; called a second time; this will almost surely be the cogserver
; atomspace. Ugh. What a mess.

(define-public cog-initial-as (cog-atomspace))
(if (eq? cog-initial-as #f)
	(begin
		(set! cog-initial-as (cog-new-atomspace))
		; Initialize a default atomspace, just to keep things sane...
		(cog-set-atomspace! cog-initial-as)))

; Load core atom types.
(load-from-path "opencog/base/core_types.scm")

; Load other grunge too.
; Some of these things could possibly be modules ...?
; ATTENTION: if you add a file here, then be sure to ALSO add it to
; ../opencog/guile/SchemeSmob.cc SchemeSmob::module_init() circa line 257

(load-from-path "opencog/base/core-docs.scm")

(load-from-path "opencog/base/utilities.scm")

(load-from-path "opencog/base/atom-cache.scm")
(load-from-path "opencog/base/apply.scm")
(load-from-path "opencog/base/tv.scm")
(load-from-path "opencog/base/file-utils.scm")
(load-from-path "opencog/base/debug-trace.scm")

; Obsolete functions
(define-public (cog-atom X) "obsolete function" '())
(define-public (cog-undefined-handle) "obsolete function" '())
