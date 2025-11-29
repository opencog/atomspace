
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
from opencog.type_constructors import *
from opencog.atomspace import tvkey, createFloatValue

# Twiddle some atoms in the atomspace
def foo(atom_a, atom_b):
    apple = Concept('Apple')
    TV = createFloatValue([0.2, 0.69])
    get_thread_atomspace().set_value(apple, tvkey, TV)
    Inheritance(atom_a, atom_b)
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

(test-end tname)

(opencog-test-end)
