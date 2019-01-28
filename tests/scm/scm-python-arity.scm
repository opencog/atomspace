
(use-modules (opencog))
(use-modules (opencog test-runner))
(use-modules (opencog python))

(opencog-test-runner)

(define tname "opencog-arity-test")
(test-begin tname)

(python-eval "
from opencog.atomspace import AtomSpace, TruthValue
from opencog.atomspace import types
def foo(atspace):
    TV = TruthValue(0.2, 0.69)
    atspace.add_node(types.ConceptNode, 'Apple', TV)
")

(python-call-with-as "foo" (cog-atomspace))
(test-assert "atom was created" (not (eq? #f (cog-node 'ConceptNode "Apple"))))

; Make sure the scheme version of Apple has the same TV on it that
; the python code placed on it.
(define strength (cog-mean (Concept "Apple")))
(test-assert "strength value is wrong" (< (abs (- 0.2 strength)) 0.00001))

(define (catch-wrong-args thunk)
  (catch #t 
    thunk
    (lambda (key . parameters)
      (format (current-error-port)
         "Expected to catch this Python exception: '~a: ~a\n" key parameters)
      "woo-hooo!!")))

(test-assert "Failed to throw when given wrong number of arguments"
	(string=? "woo-hooo!!"
	(catch-wrong-args (lambda ()
		(python-call-with-as "foo" (cog-atomspace) (Concept "Test"))))))

(test-end tname)
