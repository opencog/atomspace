from opencog.atomspace cimport Atom, AtomSpace, TruthValue, PtrHolder
from opencog.atomspace cimport cAtomSpace, cTruthValue
from opencog.atomspace cimport tv_ptr, strength_t, count_t, shared_ptr
from opencog.atomspace cimport handle_cast
from cython.operator cimport dereference as deref

from opencog.atomspace import is_a, types, create_value_by_type

def execute_atom(AtomSpace atomspace, Atom atom):
    if atom == None: raise ValueError("execute_atom atom is: None")
    cdef cValuePtr c_value_ptr = c_execute_atom(atomspace.atomspace,
                                           deref(atom.handle))
    type = deref(c_value_ptr).get_type()
    cdef PtrHolder ptr_holder = PtrHolder.create(<shared_ptr[void]&>c_value_ptr)
    return create_value_by_type(type, ptr_holder, atomspace)



def evaluate_atom(AtomSpace atomspace, Atom atom):
    if atom == None: raise ValueError("evaluate_atom atom is: None")
    cdef tv_ptr result_tv_ptr = c_evaluate_atom(atomspace.atomspace,
                                                deref(atom.handle))
    cdef cTruthValue* result_tv = result_tv_ptr.get()
    cdef strength_t strength = deref(result_tv).get_mean()
    cdef strength_t confidence = deref(result_tv).get_confidence()
    return TruthValue(strength, confidence)
