;
; OpenCog Logger module
;
; Copyright (c) 2015 OpenCog Foundation
;

(define-module (opencog logger))

; We need this to set the LTDL_LIBRARY_PATH
(use-modules (opencog))

(load-extension "liblogger" "opencog_logger_init")

; Documentation for the functions implemented as C++ code
(set-procedure-property! cog-logger-get-filename 'documentation
"
 cog-logger-get-filename
    Return the name of the current logfile.
")

(set-procedure-property! cog-logger-get-level 'documentation
"
 cog-logger-get-level
    Return the current logging level.
")

(set-procedure-property! cog-logger-set-filename! 'documentation
"
 cog-logger-set-filename! FILENAME
    Change the current logger file to FILENAME.
    Return the previous filename.
")

(set-procedure-property! cog-logger-set-level! 'documentation
"
 cog-logger-set-level! LEVEL
    Set the current logging level to LEVEL.
    Returns the previous logging level.
")

(set-procedure-property! cog-logger-set-stdout! 'documentation
"
 cog-logger-set-stdout BOOL
    If BOOL is #t, send log messages to stdout; else don't.
    Returns the previous setting.
")

(set-procedure-property! cog-logger-set-sync! 'documentation
"
 cog-logger-set-sync BOOL
    If BOOL is #t, write message to log file synchronously; else don't.
    That is, if sync is set, then the message will be written and the
    file flushed, before the log request returns. Otherwise, logging
    is carried out in a separate thread (to minimize latency impact on
    the current thread).

    Returns the previous setting.
")

(set-procedure-property! cog-logger-set-timestamp! 'documentation
"
 cog-logger-set-timestamp BOOL
    If BOOL is #t, then a timetampe will be written with each log
    message; else not.

    Returns the previous setting.
")

; Helper functions, using ice-9 format in logger functions.

(use-modules (ice-9 format))

(define (cog-logger-error msg . args)
"
 cog-logger-error MSG ARGS
    Print MSG into the logfile, at the \"error\" logging level.
    The MSG can be in any ice-9 printing format.
"
  (cog-logger-error-str (apply format #f msg args)))

(define (cog-logger-warn msg . args)
"
 cog-logger-error MSG ARGS
    Print MSG into the logfile, at the \"warning\" logging level.
    The MSG can be in any ice-9 printing format.
"
  (cog-logger-warn-str (apply format #f msg args)))

(define (cog-logger-info msg . args)
"
 cog-logger-error MSG ARGS
    Print MSG into the logfile, at the \"info\" logging level.
    The MSG can be in any ice-9 printing format.
"
  (cog-logger-info-str (apply format #f msg args)))

(define (cog-logger-debug msg . args)
  (cog-logger-debug-str (apply format #f msg args)))

(define (cog-logger-fine msg . args)
  (cog-logger-fine-str (apply format #f msg args)))

(export cog-logger-error
        cog-logger-warn
        cog-logger-info
        cog-logger-debug
        cog-logger-fine)
