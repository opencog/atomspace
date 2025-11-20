
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

; Define a python func returning a FloatValue
(python-eval "
from opencog.atomspace import AtomSpace, types, tvkey, createFloatValue
from opencog.type_constructors import get_default_atomspace


# Twiddle some atoms in the atomspace
def foo(atom_a, atom_b):
    atomspace = get_default_atomspace()
    apple = atomspace.add_node(types.ConceptNode, 'Apple')
    TV = createFloatValue([0.2, 0.69])
    apple = atomspace.set_value(apple, tvkey, TV)
    atomspace.add_link(types.InheritanceLink, [atom_a, atom_b])
    return createFloatValue([0.42, 0.24])
")


; Call the python func defined above.
(cog-execute!
	(ExecutionOutput
		(GroundedSchema "py:foo")
		(List (Concept "fruit") (Concept "banana"))))

; Make sure that Apple was created.
(test-assert "Apple atom was created"
	(not (eq? #f (cog-node 'ConceptNode "Apple"))))

; Make sure the scheme version of Apple has the same TV on it that
; the python code placed on it.
(define tvkey (Predicate "*-TruthValueKey-*"))
(test-assert "TV on Apple is wrong"
	(< (abs (- 0.2 (cog-value-ref (cog-value (Concept "Apple") tvkey) 0))) 0.00001))

; -------------------------------------------------------------------
; Test python-call-with-as to verify that the atomspace passed
; as an argument is the same one that scheme is using.
; This tests the apply_as code path (different from GroundedSchema above).

(python-eval "
from opencog.atomspace import types
from opencog.type_constructors import createFloatValue

def test_call_with_as(atomspace):
    # Create an atom in the passed atomspace
    orange = atomspace.add_node(types.ConceptNode, 'Orange')
    key = atomspace.add_node(types.PredicateNode, 'test-key')
    value = createFloatValue([3.14, 2.71])
    orange = atomspace.set_value(orange, key, value)
    return atomspace.size()
")

; Call the python function, passing the current atomspace
(python-call-with-as "test_call_with_as" (cog-atomspace))

; Verify that Orange was created in the scheme atomspace
(test-assert "Orange atom was created via python-call-with-as"
	(not (eq? #f (cog-node 'ConceptNode "Orange"))))

; Verify the FloatValue is accessible from scheme
(define orange-value (cog-value (Concept "Orange") (Predicate "test-key")))
(test-assert "FloatValue on Orange is wrong"
	(and orange-value
	     (< (abs (- 3.14 (car (cog-value->list orange-value)))) 0.00001)
	     (< (abs (- 2.71 (cadr (cog-value->list orange-value)))) 0.00001)))

(test-end tname)

(opencog-test-end)
