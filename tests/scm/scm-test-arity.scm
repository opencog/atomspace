
(use-modules (opencog) (opencog test-runner))
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
(cog-node 'ConceptNode "Apple")

; Return the strength of a simple truth value
(define (get-tv-strength tv) (cdr (assoc 'mean (cog-tv->alist tv))))

(define strength (get-tv-strength
 (cog-tv (cog-node 'ConceptNode "Apple"))))

(test-assert "python-eval is borken" (< (- 0.2 strength) 0.00001))


(define (catch-wrong-args thunk)
  (catch #t 
    thunk
    (lambda (key . parameters)
      (format (current-error-port)
              "caught throw to '~a: ~a\n" key parameters)
      "catch")))

(define failed-result
  (catch-wrong-args
   (lambda () (python-call-with-as "foo" (cog-atomspace) (cog-node 'ConceptNode "Test")))))

(test-assert "no error with wrong number of arguments" (string=? failed-result "catch"))
(test-end t)
