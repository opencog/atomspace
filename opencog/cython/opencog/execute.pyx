from opencog.atomspace cimport Atom, AtomSpace
from opencog.atomspace cimport cAtomSpace, cValuePtr
from opencog.atomspace cimport create_python_value_from_c_value

from cython.operator cimport dereference as deref

def execute_atom(AtomSpace atomspace, Atom atom):
    return atomspace.execute(atom)

def evaluate_atom(AtomSpace atomspace, Atom atom):
    if atom is None:
        raise ValueError("evaluate_atom atom is: None")
    cdef cValuePtr result_vp = c_evaluate_atom(
        atomspace.atomspace, deref(atom.handle)
    )
    return create_python_value_from_c_value(result_vp)
