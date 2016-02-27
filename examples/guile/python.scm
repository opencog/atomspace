;
; The built-in python interpreter can be invoked from scheme, as shown
; below.  It should not be confused with another concept: calling python
; functions from ExecutionOutputLinks, which is demoed in execute.scm.


; Start by loading the python module:
(use-modules (opencog) (opencog python))

; The below should print "hello! 4"
(python-eval "print ('hello! ' + str(2+2))")

; Use execfile to load python files:
(python-eval "execfile('my_py_func.py')")

; It is possible to communicate an AtomSpace from guile to python.
; This requires several steps. First, define a python function that
; takes an atomspace as an argument.  Note that standard python
; whitespace indentation is used to define a multi-line block of python
; code.
(python-eval "
from opencog.atomspace import AtomSpace, TruthValue
from opencog.atomspace import types

def foo(atspace):
    TV = TruthValue(0.42, 0.69)
    atspace.add_node(types.ConceptNode, 'Apple', TV)
")

; The (cog-atomspace) function returns the current atomspace being used
; by scheme.  We pass it to the newly-defined python function "foo":
(python-call-with-as "foo" (cog-atomspace))

; Verify that a ConceptNode called "Apple" was created, and that it has
; the correct truth value, as set in the python code:
(cog-node 'ConceptNode "Apple")
