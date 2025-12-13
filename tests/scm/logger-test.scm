#! /usr/bin/env -S guile -s
!#
;
; logger-test.scm -- Test that the logger module loads and works
;
; This is a basic sanity test to verify that the logger shared library
; is correctly linked and can be loaded.
;

(use-modules (opencog))
(use-modules (opencog test-runner))
(use-modules (opencog logger))

(opencog-test-runner)
(define tname "logger-test")
(test-begin tname)

;; Test that we can get the current log level
(test-assert "get-log-level"
   (string? (cog-logger-get-level)))

;; Test that we can set and restore the log level
(test-assert "set-log-level"
   (let ((old-level (cog-logger-get-level)))
      (cog-logger-set-level! "warn")
      (let ((new-level (cog-logger-get-level)))
         (cog-logger-set-level! old-level)
         (string=? new-level "WARN"))))

;; Test that log level enabled predicates return booleans
(test-assert "error-enabled-is-bool"
   (boolean? (cog-logger-error-enabled?)))

(test-assert "info-enabled-is-bool"
   (boolean? (cog-logger-info-enabled?)))

;; Test that we can log a message without error
(test-assert "log-info-message"
   (begin
      (cog-logger-info "Logger test message")
      #t))

;; Test flush works without error
(test-assert "flush-logger"
   (begin
      (cog-logger-flush)
      #t))

(test-end tname)

(opencog-test-end)
