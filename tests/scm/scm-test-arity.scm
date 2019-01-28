
(use-modules (opencog) )
(use-modules (opencog test-runner))
(use-modules (opencog python))

(opencog-test-runner)
(define t "opencog-arity-test")

(test-begin t)

(python-eval "
from opencog.atomspace import AtomSpace, TruthValue
from opencog.atomspace import types
def foo(atspace):
    TV = TruthValue(0.2, 0.69)
    atspace.add_node(types.ConceptNode, 'Apple', TV)
")

(python-call-with-as "foo" (cog-atomspace))
(test-assert "atom was created" (not (eq? #f (cog-node 'ConceptNode "Apple"))))

(define strength (cog-mean (cog-node 'ConceptNode "Apple")))

(test-assert "strength value is wrong" (< (- 0.2 strength) 0.00001))


(define (catch-wrong-args thunk)
  (catch #t 
    thunk
    (lambda (key . parameters)
      (format (current-error-port)
              "caught throw to '~a: ~a\n" key parameters)
      "catch")))

(define failed-result
  (catch-wrong-args
   (lambda () (python-call-with-as "foo" (cog-atomspace) (cog-new-node 'ConceptNode "Test")))))

(test-assert "no error with wrong number of arguments" (string=? failed-result "catch"))
(test-end t)
