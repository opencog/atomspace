;
; OpenCog Execution module
;
(define-module (opencog exec))

(use-modules (opencog))
(use-modules (opencog as-config))
(load-extension (string-append opencog-ext-path-exec "libexec") "opencog_exec_init")

(export cog-evaluate! cog-execute!)

(use-modules (ice-9 optargs)) ; for define*-public

; --------------------------------------------------------------------

(define*-public (cog-execute-cache! EXEC KEY
   #:optional (METADATA '()) (FRESH #f))
"
 cog-execute-cache! EXEC KEY [METADATA [FRESH]]

   Execute or return cached execution results. This is a caching version
   of the `cog-execute!` call.

   If the optional FRESH boolean flag is #f, then if there is a Value
   stored at KEY on EXEC, return that Value. The default value of FRESH
   is #f, so the default behavior is always to return the cached value.
   If the optional FRESH boolean flag is #t, or if there is no Value
   stored at KEY, then the `cog-execute!` function is called on EXEC,
   and the result is stored at KEY.

   The METADATA Atom is optional.  If it is specified, then metadata
   about the execution is placed on EXEC at the key METADATA.
   Currently, this is just a timestamp of when this execution was
   performed. The format of the meta-data is subject to change; this
   is currently an experimental feature, driven by user requirements.

   At this time, execution is synchronous. It may be worthwhile to have
   an asynchronous version of this call, where the execution is performed
   at some other time. This has not been done yet.
"
	(define val (cog-value EXEC KEY))
	(if (and (not (nil? val)) (not FRESH)) val
		(let ((newval (cog-execute! EXEC)))
			(cog-set-value! EXEC KEY newval)
			(if (not (nil? METADATA))
				(cog-set-value! EXEC METADATA (FloatValue (current-time)))
			)
			newval
		))
)

; ------------------ THE END -------------------
