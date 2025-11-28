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
; AtomSpaces are (more or less) automatically shared between scheme and
; python. This is demonstrated below.
(python-eval "
from opencog.scheme_wrapper import scheme_eval_as
from opencog.atomspace import createFloatValue
from opencog.atomspace import types

# Get the atomspace...
asp = scheme_eval_as('(cog-atomspace)')
FV = createFloatValue([0.444, 0.777])

# Do something with it ...
banana = asp.add_node(types.ConceptNode, 'Banana')
tvkey = asp.add_node(types.PredicateNode, 'My Key')
banana.set_value(tvkey, FV)
")

; As before, verify that an atom was created, as expected.
(define banana (cog-node 'ConceptNode "Banana"))
(define banana-fv (cog-value banana tvkey))
(display "Banana FloatValue: ") (display banana-fv) (newline)

; -------------------------------------------------------------------
