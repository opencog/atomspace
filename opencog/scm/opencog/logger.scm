;
; OpenCog Logger module
;
; Copyright (c) 2015 OpenCog Foundation
;

(define-module (opencog logger))

; We need this to set the LTDL_LIBRARY_PATH
(use-modules (opencog))

(load-extension "liblogger" "opencog_logger_init")

; Declare everything the C++ library provides; this avoid compile-time
; warnings when this file gets compiled.
(export
	cog-logger-get-filename-ptr
	cog-logger-get-level-ptr
	cog-logger-set-filename-ptr!
	cog-logger-set-level-ptr!
	cog-logger-set-stdout-ptr!
	cog-logger-set-sync-ptr!
	cog-logger-set-timestamp-ptr!
	cog-logger-error-ptr
	cog-logger-warn-ptr
	cog-logger-info-ptr
	cog-logger-debug-ptr
	cog-logger-fine-ptr
)

; Documentation for the functions implemented as C++ code
(set-procedure-property! cog-logger-get-filename-ptr 'documentation
"
 cog-logger-get-filename-ptr LOGGER
    Return the name of the current logfile.
")

(set-procedure-property! cog-logger-get-level-ptr 'documentation
"
 cog-logger-get-level-ptr LOGGER
    Return the current logging level.
")

(set-procedure-property! cog-logger-set-filename-ptr! 'documentation
"
 cog-logger-set-filename-ptr! LOGGER FILENAME
    Change the current LOGGER filename to FILENAME.
    Return the previous filename.
")

(set-procedure-property! cog-logger-set-level-ptr! 'documentation
"
 cog-logger-set-level! LOGGER LEVEL
    Set the current logging level to LEVEL.
    Returns the previous logging level.
")

(set-procedure-property! cog-logger-set-stdout-ptr! 'documentation
"
 cog-logger-set-stdout! LOGGER BOOL
    If BOOL is #t, send log messages to stdout; else don't.
    Returns the previous setting.
")

(set-procedure-property! cog-logger-set-sync-ptr! 'documentation
"
 cog-logger-set-sync! LOGGER BOOL
    If BOOL is #t, write message to log file synchronously; else don't.
    That is, if sync is set, then the message will be written and the
    file flushed, before the log request returns. Otherwise, logging
    is carried out in a separate thread (to minimize latency impact on
    the current thread).

    Returns the previous setting.
")

(set-procedure-property! cog-logger-set-timestamp-ptr! 'documentation
"
 cog-logger-set-timestamp! LOGGER BOOL
    If BOOL is #t, then a timetampe will be written with each log
    message; else not.

    Returns the previous setting.
")

; Helper functions, using default logger and ice-9 format in logger
; functions. TODO

(use-modules (ice-9 format))

(define (cog-logger-error msg . args)
"
 cog-logger-error MSG ARGS
    Print MSG into the logfile, at the \"error\" logging level.
    The MSG can be in any ice-9 printing format.
"
  (cog-logger-error-ptr (apply format #f msg args)))

(define (cog-logger-warn msg . args)
"
 cog-logger-error MSG ARGS
    Print MSG into the logfile, at the \"warning\" logging level.
    The MSG can be in any ice-9 printing format.
"
  (cog-logger-warn-ptr (apply format #f msg args)))

(define (cog-logger-info msg . args)
"
 cog-logger-error MSG ARGS
    Print MSG into the logfile, at the \"info\" logging level.
    The MSG can be in any ice-9 printing format.
"
  (cog-logger-info-ptr (apply format #f msg args)))

(define (cog-logger-debug msg . args)
  (cog-logger-debug-ptr (apply format #f msg args)))

(define (cog-logger-fine msg . args)
  (cog-logger-fine-ptr (apply format #f msg args)))

(export cog-logger-error
        cog-logger-warn
        cog-logger-info
        cog-logger-debug
        cog-logger-fine)
