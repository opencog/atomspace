;
; logging.scm -- Using the logger to record info, warnings and errors.
;
; This is NOT a demonstration of Atomese!  This demonstrates the
; logging subsystem, which is not a part of the AtomSpace, but is a
; part of the cogutils suite. The atomspace just provides a scheme API
; for it. This shows how to use it.
;
; Logs are written to a file called "opencog.log", by default, and
; is written to the current directory, by default.  This can be changed.
;
(use-modules (opencog) (opencog logger))

;; Look in the "opencog.log" file for this message.
(cog-logger-info "First info message")

;; Since the default log level is set to info, this message won't
;; be logged.
(cog-logger-debug "First debug message")

;; Set the log-level to debug
(cog-logger-set-level! "debug")

;; Try again -- debug messages will be logged.
(cog-logger-debug "Second debug message")

;; By default, the log file is named "opencog.log", located in the
;; current folder. That can be changed.
(cog-logger-set-filename! "opencog-scm-logger-example.log")

;; Subsequent log messages go to the newly created log file.
(cog-logger-info "Third info message")

;; Messages can be echoed to stdout, as well as the log file.
(cog-logger-set-stdout! #t)

;; Try it!
(cog-logger-info "Fourth info message")

;; Error messages automatically generate a stack trace.
(cog-logger-error "First error message (comes with the stack trace)")

;; Normally, messages are written asynchronously, so that they do not
;; slow down the main execution thread. But sometimes messages are
;; urgent -- such as those written right before a crash. For such
;; emergency situations, one can enable synchronous logging: the
;; logger will not return until the message is written.
(cog-logger-set-sync! #t)

;; The logger supports formatted printing. See
;; https://www.gnu.org/software/guile/manual/html_node/Formatted-Output.html
;; for more information.  This makes it easy to log messages about
;; Atoms and other objects.
(define Ohi (ConceptNode "O Hi!"))
(cog-logger-info "My concept: ~a" Ohi)
