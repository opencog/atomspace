;
; OpenCog Logger module
;
; Copyright (c) 2015, 2024 OpenCog Foundation
;

(define-module (opencog logger))

(use-modules (opencog))
(use-modules (opencog as-config))
(load-extension (string-append opencog-ext-path-logger "liblogger") "opencog_logger_init")

; Declare everything the C++ library provides
(export
	cog-logger-get-filename
	cog-logger-get-level
	cog-logger-get-component
	cog-logger-set-filename!
	cog-logger-set-level!
	cog-logger-set-component!
	cog-logger-set-stdout!
	cog-logger-set-sync!
	cog-logger-set-timestamp!
	cog-logger-error-enabled?
	cog-logger-warn-enabled?
	cog-logger-info-enabled?
	cog-logger-debug-enabled?
	cog-logger-fine-enabled?
	cog-logger-error
	cog-logger-warn
	cog-logger-info
	cog-logger-debug
	cog-logger-fine
	cog-logger-flush
)

;; Documentation for the functions implemented as C++ code

(set-procedure-property! cog-logger-get-filename 'documentation
"
 cog-logger-get-filename
    Return the filename of the logger.
")

(set-procedure-property! cog-logger-get-level 'documentation
"
 cog-logger-get-level
    Return the logging level.
")

(set-procedure-property! cog-logger-get-component 'documentation
"
 cog-logger-get-component
    Return the component name of the logger.
")

(set-procedure-property! cog-logger-set-filename! 'documentation
"
 cog-logger-set-filename! FILENAME
    Change the log filename to FILENAME.
    Return the previous filename.
")

(set-procedure-property! cog-logger-set-level! 'documentation
"
 cog-logger-set-level! LEVEL
    Set the logging level to LEVEL.
    Valid levels are \"fine\", \"debug\", \"info\", \"warn\" and \"error\".
    Returns the previous logging level.
")

(set-procedure-property! cog-logger-set-component! 'documentation
"
 cog-logger-set-component! COMPONENT
    Set the component name to COMPONENT.
    Returns the previous component name.
")

(set-procedure-property! cog-logger-set-stdout! 'documentation
"
 cog-logger-set-stdout! BOOL
    If BOOL is #t, send log messages to stdout; else don't.
    Returns the previous setting.
")

(set-procedure-property! cog-logger-set-sync! 'documentation
"
 cog-logger-set-sync! BOOL
    If BOOL is #t, write message to log file synchronously; else don't.
    That is, if sync is set, then the message will be written and the
    file flushed, before the log request returns. Otherwise, logging
    is carried out in a separate thread (to minimize latency impact on
    the current thread).

    Returns the previous setting.
")

(set-procedure-property! cog-logger-set-timestamp! 'documentation
"
 cog-logger-set-timestamp! BOOL
    If BOOL is #t, then a timestamp will be written with each log
    message; else not.

    Returns the previous setting.
")

(set-procedure-property! cog-logger-flush 'documentation
"
 cog-logger-flush
    Flush any pending logging.
")

; Helper functions using ice-9 format in logger functions.

(use-modules (ice-9 format))

(define (cog-logger-error msg . args)
"
 cog-logger-error MSG ARGS
    Print MSG into the log file, at the \"error\" logging level.
    The MSG can be in any ice-9 printing format.
"
  (cog-logger-error-str (apply format #f msg args)))

(define (cog-logger-warn msg . args)
"
 cog-logger-warn MSG ARGS
    Print MSG into the log file, at the \"warn\" logging level.
    The MSG can be in any ice-9 printing format.
"
  (cog-logger-warn-str (apply format #f msg args)))

(define (cog-logger-info msg . args)
"
 cog-logger-info MSG ARGS
    Print MSG into the log file, at the \"info\" logging level.
    The MSG can be in any ice-9 printing format.
"
  (cog-logger-info-str (apply format #f msg args)))

(define (cog-logger-debug msg . args)
"
 cog-logger-debug MSG ARGS
    Print MSG into the log file, at the \"debug\" logging level.
    The MSG can be in any ice-9 printing format.
"
  (cog-logger-debug-str (apply format #f msg args)))

(define (cog-logger-fine msg . args)
"
 cog-logger-fine MSG ARGS
    Print MSG into the log file, at the \"fine\" logging level.
    The MSG can be in any ice-9 printing format.
"
  (cog-logger-fine-str (apply format #f msg args)))
