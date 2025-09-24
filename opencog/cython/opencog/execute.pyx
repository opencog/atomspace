from opencog.atomspace cimport Atom, AtomSpace
from opencog.atomspace cimport cAtomSpace, cTruthValue
from opencog.atomspace cimport tv_ptr
from opencog.atomspace cimport create_python_value_from_c_value

from cython.operator cimport dereference as deref

from opencog.type_constructors import TruthValue

def execute_atom(AtomSpace atomspace, Atom atom):
    return atomspace.execute(atom)

def evaluate_atom(AtomSpace atomspace, Atom atom):
    if atom is None:
        raise ValueError("evaluate_atom atom is: None")
    cdef tv_ptr result_tv_ptr = c_evaluate_atom(
        atomspace.atomspace, deref(atom.handle)
    )
    cdef cTruthValue* result_tv = result_tv_ptr.get()
    cdef double strength = deref(result_tv).get_mean()
    cdef double confidence = deref(result_tv).get_confidence()
    return TruthValue(strength, confidence)
