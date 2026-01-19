;
; Main Atomese guile module
;
; When this module is loaded from the guile prompt, it sets up all of
; the Atomese infrastructure, including a default AtomSpace.
;
; To use, say this from the guile prompt:
; (use-modules (opencog))
;
;
; The default guile locale is "C", the POSIX locale that is "inherited
; by default" from the shell. But all Atomese string processing is done
; in utf8; so we have utf8 being mandatory. Note that this is still
; required for guile-3.0.x and will likely stay like this "forever".
; See https://github.com/opencog/opencog/issues/937
(setlocale LC_CTYPE "")
(setlocale LC_NUMERIC "C")

(define-module (opencog))

; When run from the build env, without saying `make install` to install
; into the main filesystem, then the expression
;     (use-modules (opencog as-config))
; will fail because (duh) it's not installed. So, instead, we manually
; hunt for it, and use the version found in the build tree.  This is
; a hairy hack needed to be able to run unit tests without installing.
; Note: FYI: the build version of `as-config.scm` is different from the
; install version of `asconfig.scm` -- the two hold different filepaths.
(if (resolve-module (list 'opencog 'as-config) #:ensure #f)
	(use-modules (opencog as-config))
	(load-from-path "opencog/as-config.scm"))

; Use module-ref to avoid "possibly unbound variable" warning at compile time.
(define opencog-ext-path-smob
	(module-ref (resolve-module '(opencog as-config)) 'opencog-ext-path-smob))

(load-extension (string-append opencog-ext-path-smob "libsmob") "opencog_guile_init")

; List everything to be exported from the C++ code i.e. from libsmob,
; as otherwise guile generates warnings about "possibly unbound variable"
; when these are touched in the various scm files.
(export
cog-atom
cog-atom-less?
cog-atomspace
cog-atomspace-clear
cog-atomspace-cow!
cog-atomspace-cow?
cog-atomspace-env
cog-atomspace-readonly?
cog-atomspace-ro!
cog-atomspace-rw!
cog-count-atoms
cog-equal?
cog-extract!
cog-extract-recursive!
cog-get-subtypes
cog-get-types
cog-handle
cog-incoming-by-type
cog-incoming-set
cog-incoming-size
cog-incoming-size-by-type
cog-keys
cog-link
cog-map-type
cog-name
cog-new-ast
cog-new-atom
cog-new-atomspace
cog-new-link
cog-new-node
cog-new-value
cog-node
cog-pop-atomspace
cog-push-atomspace
cog-set-atomspace!
cog-set-value!
cog-subtype?
cog-type
cog-type->int
cog-value
cog-value->list
cog-value-ref

; Deprecated; used by the matrix code ... which itself is deprecated.
cog-inc-value!
cog-update-value!
)

; Print C++ exceptions so that they are readable.
(define (cpp-exception-printer port key args default-printer)
	(format port "Atomspace C++ exception:\n~A\n" args))

; set-exception-printer! is exposed by ice-9/boot-9
(set-exception-printer! 'C++-EXCEPTION cpp-exception-printer)

; Create a global to hold the atomspace ... to (try to) prevent guile
; GC from collecting it.
;
; There are scenarios where python might run before scheme, or where
; the cogserver may run, and create an AtomSpace; we want to use that
; one, so that everyone agrees about what "this" AtomSpace is. So we
; go look to see if the current AtomSpace is already set. If it is not,
; then create one.
;
; This is kind-of squonky, I don't entirely like it. However, the
; alternative is to do this in the c++ code, and that's uglier. So
; keep it fresh, and do it here.
;
(define-public cog-initial-as (cog-atomspace))
(if (nil? cog-initial-as)
	(begin
		(set! cog-initial-as (cog-new-atomspace))
		(cog-set-atomspace! cog-initial-as)))

; A very special association-list ctor.
(define-public (alist . x) (list 'alist x))

; Load core atom types.
(include-from-path "opencog/base/core_types.scm")

; Perform execution by triggering
(define-public (cog-execute! x) (TriggerLink x))

; Misc old grunge.
; ATTENTION: if you add/remove a file here, then be sure to ALSO
; do the same at
;    guile/SchemeSmob.cc SchemeSmob::module_init() circa line 260

(include-from-path "opencog/base/core-docs.scm")
(include-from-path "opencog/base/utilities.scm")
(include-from-path "opencog/base/atom-cache.scm")

; --- The End ---
