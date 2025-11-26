;
; frame-incoming-test.scm
; Verify that getIncomingSet() works on AtomSpaces.
;
(use-modules (srfi srfi-1))
(use-modules (opencog) (opencog test-runner))

(opencog-test-runner)

; -------------------------------------------------------------------
; Common setup, used by all tests.

(define level0 (cog-atomspace))
(define level1a (AtomSpace level0))
(define level1b (AtomSpace level0))
(define level1c (AtomSpace level0))
(define level2a (AtomSpace level1a level1b level1c))
(define level2b (AtomSpace level1a))

; -------------------------------------------------------------------
; Test that incoming sets work.

(define space-inc "test incoming")
(test-begin space-inc)

(test-equal "inc-size" 3 (cog-incoming-size level0))
(test-equal "inc-set" 3 (length (cog-incoming-set level0)))
(test-equal "inc-eq1b" level2a (car (cog-incoming-set level1b)))
(test-equal "inc-eq1c" level2a (car (cog-incoming-set level1c)))

(test-equal "inc-size1a" 2 (cog-incoming-size level1a))
(test-equal "inc-size1b" 1 (cog-incoming-size level1b))
(test-equal "inc-size1c" 1 (cog-incoming-size level1c))

(test-end space-inc)

; -------------------------------------------------------------------
(opencog-test-end)
