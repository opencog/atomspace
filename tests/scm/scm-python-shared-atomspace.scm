
(use-modules (opencog))
(use-modules (opencog exec))
(use-modules (opencog python))
(use-modules (opencog test-runner))

(opencog-test-runner)

; Explicitly test that the atomspace that python is using is
; the same one that guile is using. Without this, atoms created
; by python scripts will go into some crazy private atomspace
; where they cannot be found by anyone ...

(define tname "python-guile-shared-atomspace-test")
(test-begin tname)

; Define a python func returning a TV
(python-eval "
from opencog.atomspace import AtomSpace, types
from opencog.type_constructors import get_type_ctor_atomspace, TruthValue


# Twiddle some atoms in the atomspace
def foo(atom_a, atom_b):
    atomspace = get_type_ctor_atomspace()
    TV = TruthValue(0.2, 0.69)
    atomspace.add_node(types.ConceptNode, 'Apple', TV)
    atomspace.add_link(types.InheritanceLink, [atom_a, atom_b])
    return TruthValue(0.42, 0.24)
")


; Call the python func defined above.
(cog-evaluate!
	(Evaluation
		(GroundedPredicate "py:foo")
		(List (Concept "fruit") (Concept "banana"))))

; Make sure that Apple was created.
(test-assert "Apple atom was created"
	(not (eq? #f (cog-node 'ConceptNode "Apple"))))

; Make sure the scheme version of Apple has the same TV on it that
; the python code placed on it.
(test-assert "TV on Apple is wrong"
	(< (abs (- 0.2 (cog-mean (Concept "Apple")))) 0.00001))

(test-end tname)

(opencog-test-end)
