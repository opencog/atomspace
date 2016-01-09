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

; libsmob won't be found unless we setenv where to find it!
(setenv "LTDL_LIBRARY_PATH"
	(if (getenv "LTDL_LIBRARY_PATH")
		(string-append (getenv "LTDL_LIBRARY_PATH") ":/usr/local/lib/opencog")
		"/usr/local/lib/opencog"))

; Work-around another common usability issue...
(add-to-load-path "/usr/local/share/opencog/scm")

(define-module (opencog))
(load-extension "libsmob" "opencog_guile_init")

(use-modules (system base compile))

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
; The remaining atom types from the cogserver are in (opencog atom-types)
(load-from-path "core_types.scm")

; Load other grunge too.
; Some of these things should probably be modules ...?
(load-from-path "config.scm")

(load-from-path "core-docs.scm")

(load-from-path "utilities.scm")

(load-from-path "apply.scm")
(load-from-path "av-tv.scm")
(load-from-path "file-utils.scm")
(load-from-path "debug-trace.scm")
