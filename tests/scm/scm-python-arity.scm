
(use-modules (opencog))
(use-modules (opencog exec))
(use-modules (opencog python))
(use-modules (opencog test-runner))

(opencog-test-runner)

(define tname "python-arity-test")
(test-begin tname)

; Define a python func returning a TV
(python-eval "
from opencog.atomspace import AtomSpace, TruthValue
from opencog.atomspace import types

# Hack around bug #2020
shared_as = AtomSpace()
def do_set_atomspace(sas) :
    global shared_as
    shared_as = sas

# Twiddle some atoms in the atomspace
def foo(atom_a, atom_b) :
    TV = TruthValue(0.2, 0.69)
    shared_as.add_node(types.ConceptNode, 'Apple', TV)
    shared_as.add_link(types.InheritanceLink, [atom_a, atom_b])
    return TruthValue(0.42, 0.24)
")

(python-call-with-as "do_set_atomspace" (cog-atomspace))

; Call the python func defined above.
(define returned-tv
	(cog-evaluate!
		(Evaluation
			(GroundedPredicate "py:foo")
			(List (Concept "fruit") (Concept "banana")))))

; Make sure that Apple was created.
(test-assert "Apple atom was created"
	(not (eq? '() (cog-node 'ConceptNode "Apple"))))

; Make sure the scheme version of Apple has the same TV on it that
; the python code placed on it.
(test-assert "TV on Apple is wrong"
	(< (abs (- 0.2 (cog-mean (Concept "Apple")))) 0.00001))

(test-assert "returned TV is wrong"
	(< (abs (- 0.42 (cog-tv-mean returned-tv))) 0.00001))

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
	(eq? (SimpleTruthValue 0.42 0.24)
		(catch-wrong-args (lambda ()
			(cog-evaluate!
				(Evaluation
					(GroundedPredicate "py:foo")
					(List (Concept "fruit") (Concept "banana"))))))))

(test-assert "Failed to throw when given too few arguments"
	(string=? "woo-hooo!!"
		(catch-wrong-args (lambda ()
			(cog-evaluate!
				(Evaluation
					(GroundedPredicate "py:foo")
					(List (Concept "fruit"))))))))

(test-assert "Failed to throw when given too many arguments"
	(string=? "woo-hooo!!"
		(catch-wrong-args (lambda ()
			(cog-evaluate!
				(Evaluation
					(GroundedPredicate "py:foo")
					(List
						(Concept "fruit-stuff")
						(Concept "banana")
						(Concept "orange"))))))))

(test-end tname)
