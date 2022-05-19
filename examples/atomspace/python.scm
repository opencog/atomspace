;
; python.scm -- mixing Python code with Scheme
;
; The built-in python interpreter can be invoked from scheme, as shown
; below.  It should not be confused with another concept: calling python
; functions from ExecutionOutputLinks, which is demoed in `execute.scm`.


; Start by loading the python module:
(use-modules (opencog) (opencog python))

; The below should print "hello! 4"
(python-eval "print ('hello! ' + str(2+2))")

; Use exec(open()) to load python files:
(python-eval "exec(open('my_py_func.py').read())")

; -------------------------------------------------------------------
; It is possible to communicate an AtomSpace from guile to python.
; There are two ways to do this: one way is to tell python what the
; atomspace is; the other is to have python ask what it is. The first
; example shows how to "tell" python: it calls a python function,
; supplying some atomspace as an argument.
;
; This requires several steps. First, define a python function that
; takes an atomspace as an argument.  Note that standard python
; whitespace indentation is used to define a multi-line block of python
; code.
(python-eval "
from opencog.atomspace import AtomSpace, createTruthValue
from opencog.atomspace import types

def foo(atspace):
    TV = createTruthValue(0.42, 0.69)
    atspace.add_node(types.ConceptNode, 'Apple', TV)
")

; The (cog-atomspace) function returns the current atomspace being used
; by scheme.  We pass it to the newly-defined python function "foo":
(python-call-with-as "foo" (cog-atomspace))

; Verify that a ConceptNode called "Apple" was created, and that it has
; the correct truth value, as set in the python code:
(cog-node 'ConceptNode "Apple")

; -------------------------------------------------------------------
; The other way is to have python ask scheme to tell it about some
; atomspace.  This is shown below. As always, recall that
; (cog-atomspace) simply returns the AtomSpace that scheme is currently
; using (in the current thread).
(python-eval "
from opencog.scheme_wrapper import scheme_eval_as
from opencog.atomspace import createTruthValue
from opencog.atomspace import types

# Get the atomspace...
asp = scheme_eval_as('(cog-atomspace)')
TV = createTruthValue(0.444, 0.777)

# Do something with it ...
asp.add_node(types.ConceptNode, 'Banana', TV)
")

; As before, verify that an atom was created, as expected.
(cog-node 'ConceptNode "Banana")

; -------------------------------------------------------------------
