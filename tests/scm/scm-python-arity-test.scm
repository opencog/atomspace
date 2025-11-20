
(use-modules (opencog))
(use-modules (opencog exec))
(use-modules (opencog python))
(use-modules (opencog test-runner))

(opencog-test-runner)

(define tname "python-arity-test")
(test-begin tname)

; Check to make sure that python actually throws an exception,
; when it gets the wrong number of arguments passed to it.

; Define a python func returning a boolean
(python-eval "
from opencog.atomspace import AtomSpace, types
from opencog.type_constructors import FloatValue, PredicateNode


# Twiddle some atoms in the atomspace
def foo(atom_a, atom_b) :
    atomspace = atom_a.atomspace
    key = PredicateNode('my-key')
    fv = FloatValue([0.2, 0.69])
    apple = atomspace.add_node(types.ConceptNode, 'Apple')
    appl = atomspace.set_value(apple, key, fv)
    atomspace.add_link(types.InheritanceLink, [atom_a, atom_b])
    return True
")

; Call the python func defined above.
(define returned-bool
	(cog-execute!
		(Evaluation
			(GroundedPredicate "py:foo")
			(List (Concept "fruit") (Concept "banana")))))

; Make sure that Apple was created.
(test-assert "Apple atom was created"
	(not (eq? '() (cog-node 'ConceptNode "Apple"))))

; Make sure the scheme version of Apple has the same FloatValue on it that
; the python code placed on it.
(define apple-value (cog-value (Concept "Apple") (Predicate "my-key")))
(test-assert "FloatValue on Apple is wrong"
	(and apple-value
	     (< (abs (- 0.2 (car (cog-value->list apple-value)))) 0.00001)
	     (< (abs (- 0.69 (cadr (cog-value->list apple-value)))) 0.00001)))

(test-assert "returned boolean is wrong"
	(equal? (BoolValue #t) returned-bool))

; Handy-dandy try-catch wrapper
(define (catch-wrong-args thunk)
	(catch #t
		thunk
		(lambda (key . parameters)
			(format (current-error-port)
				"Expected to catch this Python exception: '~a: ~a\n"
				key parameters)
			"woo-hooo!!")))

; Make sure that the handy-dandy try-catch wrapper is working.
(test-assert "Threw exception even when given the right number of arguments"
	(equal? (BoolValue #t)
		(catch-wrong-args (lambda ()
			(cog-execute!
				(Evaluation
					(GroundedPredicate "py:foo")
					(List (Concept "fruit") (Concept "banana"))))))))

(test-assert "Failed to throw when given too few arguments"
	(string=? "woo-hooo!!"
		(catch-wrong-args (lambda ()
			(cog-execute!
				(Evaluation
					(GroundedPredicate "py:foo")
					(List (Concept "fruit"))))))))

(test-assert "Failed to throw when given too many arguments"
	(string=? "woo-hooo!!"
		(catch-wrong-args (lambda ()
			(cog-execute!
				(Evaluation
					(GroundedPredicate "py:foo")
					(List
						(Concept "fruit-stuff")
						(Concept "banana")
						(Concept "orange"))))))))

(test-end tname)

(opencog-test-end)
