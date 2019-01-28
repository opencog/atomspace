
(use-modules (opencog))
(use-modules (opencog test-runner))
(use-modules (opencog exec))
(use-modules (opencog python))

(opencog-test-runner)

(define tname "python-guile-shared-atomspace-test")
(test-begin tname)

; Define a python func returning a TV
(python-eval "
from opencog.atomspace import AtomSpace, TruthValue
from opencog.atomspace import types
def foo(atom_a, atom_b):
    asp = AtomSpace()
    TV = TruthValue(0.2, 0.69)
    asp.add_node(types.ConceptNode, 'Apple', TV)
    asp.add_link(types.InheritanceLink, [atom_a, atom_b])
    return TruthValue(0.42, 0.24)
")

; Call the python func defined above.
(cog-evaluate!
	(Evaluation
		(GroundedPredicate "py:foo")
		(List (Concept "fruit") (Concept "banana")))))

; Make sure that Apple was created.
(test-assert "Apple atom was created"
	(not (eq? #f (cog-node 'ConceptNode "Apple"))))

; Make sure the scheme version of Apple has the same TV on it that
; the python code placed on it.
(test-assert "TV on Apple is wrong"
	(< (abs (- 0.2 (cog-mean (Concept "Apple")))) 0.00001))

(test-end tname)
