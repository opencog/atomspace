;
; Main OpenCog guile module
;
; When this module is loaded from the guile prompt, it sets up all of
; the opencog infrastructure, including a default atomspace.
;
; To use, say this from the guile prompt:
; (load-modules (opencog))
;
;
; This should result in a utf8 locale being used!
; See https://github.com/opencog/opencog/issues/937
(setlocale LC_CTYPE "")

; libsmob won't be found unless we setenv where to find it!
(setenv "LTDL_LIBRARY_PATH" "/usr/local/lib/opencog")

; Work-around another common usability issue...
(add-to-load-path "/usr/local/share/opencog/scm")

(define-module (opencog))
(load-extension "libsmob" "opencog_guile_init")

(use-modules (system base compile))

; Initialze a default atomspace, just to keep things sane...
; The below is safe, because this module runs at most only once
; (if invoked from the guile shell, as (load-modules (opencog)) )
; or zero times, if invoked from the cogserver shell. The cogserver
; refuses to run this; the cogserver main atomspace is never clobbered.
;
(cog-set-atomspace! (cog-new-atomspace))

; Load core atom types
; The remaining atom types from the cogserver are in (opencog atom-types)
(load-from-path "core_types.scm")

; Load other grunge too
;
; Lots of these things should probably be modules ...
;
; Also they need to be defined "public" to be useable in guile, except
; for utilities that uses (export-utilities)
(load-from-path "config.scm")

(load-from-path "utilities.scm")
(export-utilities)

(load-from-path "apply.scm")
(load-from-path "av-tv.scm")
(load-from-path "file-utils.scm")
(load-from-path "persistence.scm")
