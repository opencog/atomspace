(define-module (opencog test-runner)
  #:use-module (srfi srfi-64)
  #:re-export (test-begin test-assert test-equal test-end)
  #:export (opencog-test-runner opencog-test-end))

(use-modules (opencog))

(define (opencog-test-runner)
"
  opencog-test-runner

  Sets a new test-runner that is based on test-runner-simple, by wrapping
  test-on-final-simple so as to return non-zero exit codes when
  there are test failures. This allows cmake's ctest to report the failures
  properly, provided (test-end) is called.

  For using the runner follow the following pattern in test-scripts

    (opencog-test-runner)

    (test-begin \"Name of the test suite\")
    (test-assert ...)
    ...
    ...
    (test-end \"Name of the test suite\")
    ...
    (opencog-test-end)

  For details on the apis for writing srfi-64 test-suites, see
  https://srfi.schemers.org/srfi-64/srfi-64.html
"
  (define (new-final runner)
    ((test-runner-on-final (test-runner-simple)) runner)
    (let ((tot-fail (+ (test-runner-fail-count runner)
                       (test-runner-xfail-count runner))))
      (when (< 0 tot-fail)
        (display "%%%% Exiting test suite with failures!\n")
        (exit tot-fail))))

  (let ((runner (test-runner-simple)))
    (test-runner-on-final! runner new-final)

    ; Set the factory for the new runner.
    (test-runner-factory (lambda () runner))
  )
)

(define (opencog-test-end)
"
  opencog-test-end

  Exit guile, setting the exit code to indicate pass/fail.
"

 ; Failures exited previously; if we are here, everything passed.
 (display "%%%% Exiting test suite; all tests passed!\n")
 (exit 0)
)
