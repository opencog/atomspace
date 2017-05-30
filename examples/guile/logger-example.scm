;; Example of opencog logger scheme module

(use-modules (opencog logger))

(cog-logger-info "First info message")

;; Likely the log level is set to info, this message won't be logged
(cog-logger-debug "First debug message")

;; Let's set it to debug
(cog-logger-set-level! "debug")

;; Now we can log a debug message
(cog-logger-debug "Second debug message")

;; By default the log file is opencog.log, located in the current
;; folder. Let's change that
(cog-logger-set-filename! "opencog-scm-logger-example.log")

;; Now log message go to the newly created log file
(cog-logger-info "Third info message")

;; Actually, screw those log files, I want the messages to be printed
;; to the stdout
(cog-logger-set-stdout! #t)

;; Let's see (well it is still logged to the current log file as well)
(cog-logger-info "Fourth info message")

;; I'd like to have the stack trace on top of the log message, the
;; error level provides that
(cog-logger-error "First error message (comes with the stack trace)")

;; Finally, in case you want the message to be logged exactly as the
;; logger is called you can set the sync flag to true
(cog-logger-set-sync! #t)

;; The logger supports scheme format (see
;; https://www.gnu.org/software/guile/manual/html_node/Formatted-Output.html
;; for more information), that way you can easily log messages about
;; atoms or any object.
(define A (ConceptNode "A"))
(cog-logger-info "My concept ~a" A)
