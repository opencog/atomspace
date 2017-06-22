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
	cog-default-logger
	cog-ure-logger
	cog-logger-get-filename-of-logger
	cog-logger-get-level-of-logger
	cog-logger-get-component-of-logger
	cog-logger-set-filename-of-logger!
	cog-logger-set-level-of-logger!
	cog-logger-set-component-of-logger!
	cog-logger-set-stdout-of-logger!
	cog-logger-set-sync-of-logger!
	cog-logger-set-timestamp-of-logger!
	cog-logger-error-of-logger
	cog-logger-warn-of-logger
	cog-logger-info-of-logger
	cog-logger-debug-of-logger
	cog-logger-fine-of-logger
)

;; Documentation for the functions implemented as C++ code

(set-procedure-property! cog-default-logger 'documentation
"
 cog-default-logger
    Return the default logger.
")

(set-procedure-property! cog-ure-logger 'documentation
"
 cog-ure-logger
    Return the rule-engine logger.
")

(set-procedure-property! cog-logger-get-filename-of-logger 'documentation
"
 cog-logger-get-filename-of-logger LOGGER
    Return the filename of LOGGER.
")

(set-procedure-property! cog-logger-get-level-of-logger 'documentation
"
 cog-logger-get-level-of-logger LOGGER
    Return the logging level of LOGGER.
")

(set-procedure-property! cog-logger-get-component-of-logger 'documentation
"
 cog-logger-get-component-of-logger LOGGER
    Return the component name of LOGGER.
")

(set-procedure-property! cog-logger-set-filename-of-logger! 'documentation
"
 cog-logger-set-filename-of-logger! LOGGER FILENAME
    Change the filename of LOGGER to FILENAME.
    Return the previous filename.
")

(set-procedure-property! cog-logger-set-level-of-logger! 'documentation
"
 cog-logger-set-level! LOGGER LEVEL
    Set the logging level of LOGGER to LEVEL.
    Returns the previous logging level.
")

(set-procedure-property! cog-logger-set-stdout-of-logger! 'documentation
"
 cog-logger-set-stdout! LOGGER BOOL
    If BOOL is #t, send log messages to stdout; else don't.
    Returns the previous setting.
")

(set-procedure-property! cog-logger-set-sync-of-logger! 'documentation
"
 cog-logger-set-sync! LOGGER BOOL
    If BOOL is #t, write message to log file synchronously; else don't.
    That is, if sync is set, then the message will be written and the
    file flushed, before the log request returns. Otherwise, logging
    is carried out in a separate thread (to minimize latency impact on
    the current thread).

    Returns the previous setting.
")

(set-procedure-property! cog-logger-set-timestamp-of-logger! 'documentation
"
 cog-logger-set-timestamp! LOGGER BOOL
    If BOOL is #t, then a timestamp will be written with each log
    message; else not.

    Returns the previous setting.
")

; Helper functions, using default logger and ice-9 format in logger
; functions.

(use-modules (ice-9 format))

(define (add-default-logger . args)
"
 add-defaultlogger [LOGGER] [ARG1] ... [ARGn]
    Return the list of argument with the default logger
    prepended to it if LOGGER was not provided.
"
  (if (and (< 0 (length args)) (cog-logger? (car args)))
      args
      (cons (cog-default-logger) args)))

(define (cog-logger-get-filename . args)
"
 cog-logger-get-filename [LOGGER]
    Return the filename of LOGGER if provided.
    If not provided then use the default logger.
"
  (apply cog-logger-get-filename-of-logger (apply add-default-logger args)))

(define (cog-logger-get-level . args)
"
 cog-logger-get-level [LOGGER]
    Return the logging level of LOGGER if provided.
    If not provided then use the default logger.
"
  (apply cog-logger-get-level-of-logger (apply add-default-logger args)))

(define (cog-logger-get-component . args)
"
 cog-logger-get-level [LOGGER]
    Return the component name of LOGGER if provided.
    If not provided then use the default logger.
"
  (apply cog-logger-get-component-of-logger (apply add-default-logger args)))

(define (cog-logger-set-filename! . args)
"
 cog-logger-set-filename! [LOGGER] FILENAME
    Change the filename of LOGGER to FILENAME.
    If LOGGER is not provided then use the default logger.

    Return the previous filename.
"
  (apply cog-logger-set-filename-of-logger! (apply add-default-logger args)))

(define (cog-logger-set-level! . args)
"
 cog-logger-set-level! [LOGGER] LEVEL
    Change the logging level of LOGGER to LEVEL.
    If LOGGER is not provided then use the default logger.

    Returns the previous logging level.
"
  (apply cog-logger-set-level-of-logger! (apply add-default-logger args)))

(define (cog-logger-set-component! . args)
"
 cog-logger-set-component! [LOGGER] COMPONENT
    Change the component name of LOGGER to COMPONENT.
    If LOGGER is not provided then use the default logger.

    Returns the previous component name.
"
  (apply cog-logger-set-component-of-logger! (apply add-default-logger args)))

(define (cog-logger-set-stdout! . args)
"
 cog-logger-set-stdout! [LOGGER] STDOUT
    If BOOL is #t, send log messages to stdout; else don't.
    If LOGGER is not provided then use the default logger

    Returns the previous setting.
"
  (apply cog-logger-set-stdout-of-logger! (apply add-default-logger args)))

(define (cog-logger-set-sync! . args)
"
 cog-logger-set-sync! [LOGGER] BOOL
    If BOOL is #t, write message to log file synchronously; else don't.
    That is, if sync is set, then the message will be written and the
    file flushed, before the log request returns. Otherwise, logging
    is carried out in a separate thread (to minimize latency impact on
    the current thread).
    If LOGGER is not provided then use the default logger.

    Returns the previous setting.
"
  (apply cog-logger-set-sync-of-logger! (apply add-default-logger args)))

(define (cog-logger-set-timestamp! . args)
"
 cog-logger-set-timestamp! [LOGGER] BOOL
    If BOOL is #t, then a timestamp will be written with each log
    message; else not.
    If LOGGER is not provided then use the default logger.

    Returns the previous setting.
"
  (apply cog-logger-set-timestamp-of-logger! (apply add-default-logger args)))

(define (cog-logger-error . args)
"
 cog-logger-error [LOGGER] MSG ARGS
    Print MSG into the log file, at the \"error\" logging level.
    The MSG can be in any ice-9 printing format.
    If LOGGER is not provided then use the default logger.
"
  (apply cog-logger-error-with-format (apply add-default-logger args)))

(define (cog-logger-error-with-format logger msg . args)
"
 cog-logger-error LOGGER MSG ARGS
    Print MSG into the log file, at the \"error\" logging level.
    The MSG can be in any ice-9 printing format.
"
  (cog-logger-error-of-logger logger (apply format #f msg args)))

(define (cog-logger-warn . args)
"
 cog-logger-warn [LOGGER] MSG ARGS
    Print MSG into the log file, at the \"warn\" logging level.
    The MSG can be in any ice-9 printing format.
    If LOGGER is not provided then use the default logger.
"
  (apply cog-logger-warn-with-format (apply add-default-logger args)))

(define (cog-logger-warn-with-format logger msg . args)
"
 cog-logger-warn LOGGER MSG ARGS
    Print MSG into the log file, at the \"warn\" logging level.
    The MSG can be in any ice-9 printing format.
"
  (cog-logger-warn-of-logger logger (apply format #f msg args)))

(define (cog-logger-info . args)
"
 cog-logger-info [LOGGER] MSG ARGS
    Print MSG into the log file, at the \"info\" logging level.
    The MSG can be in any ice-9 printing format.
    If LOGGER is not provided then use the default logger.
"
  (apply cog-logger-info-with-format (apply add-default-logger args)))

(define (cog-logger-info-with-format logger msg . args)
"
 cog-logger-info LOGGER MSG ARGS
    Print MSG into the log file, at the \"info\" logging level.
    The MSG can be in any ice-9 printing format.
"
  (cog-logger-info-of-logger logger (apply format #f msg args)))

(define (cog-logger-debug . args)
"
 cog-logger-debug [LOGGER] MSG ARGS
    Print MSG into the log file, at the \"debug\" logging level.
    The MSG can be in any ice-9 printing format.
    If LOGGER is not provided then use the default logger.
"
  (apply cog-logger-debug-with-format (apply add-default-logger args)))

(define (cog-logger-debug-with-format logger msg . args)
"
 cog-logger-debug LOGGER MSG ARGS
    Print MSG into the log file, at the \"debug\" logging level.
    The MSG can be in any ice-9 printing format.
"
  (cog-logger-debug-of-logger logger (apply format #f msg args)))

(define (cog-logger-fine . args)
"
 cog-logger-fine [LOGGER] MSG ARGS
    Print MSG into the log file, at the \"fine\" logging level.
    The MSG can be in any ice-9 printing format.
    If LOGGER is not provided then use the default logger.
"
  (apply cog-logger-fine-with-format (apply add-default-logger args)))

(define (cog-logger-fine-with-format logger msg . args)
"
 cog-logger-fine LOGGER MSG ARGS
    Print MSG into the log file, at the \"fine\" logging level.
    The MSG can be in any ice-9 printing format.
"
  (cog-logger-fine-of-logger logger (apply format #f msg args)))

(export cog-logger-get-filename
        cog-logger-get-level
        cog-logger-get-component
        cog-logger-set-filename!
        cog-logger-set-level!
        cog-logger-set-component!
        cog-logger-set-stdout!
        cog-logger-set-sync!
        cog-logger-set-timestamp!
        cog-logger-error
        cog-logger-warn
        cog-logger-info
        cog-logger-debug
        cog-logger-fine)
