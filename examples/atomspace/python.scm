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
; AtomSpaces are automatically shared between scheme and python. This is
; demonstrated below.
(python-eval "
from opencog.type_constructors import *
from opencog.atomspace import createFloatValue

# Create atoms in the current (scheme) atomspace
banana = Concept('Banana')
mykey = Predicate('My Key')

# Attach a value
FV = createFloatValue([0.444, 0.777])
get_thread_atomspace().set_value(banana, mykey, FV)
")

; Verify that the "Bannana" atom was created, as expected.
(define banana (cog-node 'Concept "Banana"))
(format #t "The AtomSpace now contains this: ~A\n" banana)
(define banana-fv (cog-value banana (Predicate "My Key")))
(format #t "The Value on 'banana' is: ~A\n" banana-fv)

; THE END. That's All, Folks!
; -------------------------------------------------------------------
