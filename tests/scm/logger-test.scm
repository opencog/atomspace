#! /usr/bin/env guile
-s
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
(use-modules (ice-9 rdelim))

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

;; Test that logging to a file works
(define test-logfile "/tmp/opencog-logger-test.log")
(define test-message "Logger unit test marker message 12345")

(test-assert "log-to-file"
   (begin
      ;; Set up test logging
      (cog-logger-set-filename! test-logfile)
      (cog-logger-set-level! "info")
      (cog-logger-set-sync! #t)

      ;; Write test message
      (cog-logger-info test-message)
      (cog-logger-flush)

      ;; Read the log file and check for our message
      (let* ((port (open-input-file test-logfile))
             (content (read-string port))
             (found (string-contains content test-message)))
         (close-port port)

         ;; Clean up test file
         (delete-file test-logfile)

         ;; Return whether message was found
         found)))

(test-end tname)

(opencog-test-end)
