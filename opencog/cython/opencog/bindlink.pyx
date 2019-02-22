from opencog.atomspace cimport Atom, AtomSpace, TruthValue
from opencog.atomspace cimport cHandle, cAtomSpace, cTruthValue
from opencog.atomspace cimport tv_ptr, strength_t, count_t
from opencog.atomspace cimport handle_cast
from cython.operator cimport dereference as deref

def execute_atom(AtomSpace atomspace, Atom atom):
    if atom == None: raise ValueError("execute_atom atom is: None")
    cdef cValuePtr c_value_ptr = c_execute_atom(atomspace.atomspace,
                                           deref(atom.handle))
    cdef cHandle c_result = handle_cast(c_value_ptr)
    return Atom.createAtom(c_result, atomspace)

def evaluate_atom(AtomSpace atomspace, Atom atom):
    if atom == None: raise ValueError("evaluate_atom atom is: None")
    cdef tv_ptr result_tv_ptr = c_evaluate_atom(atomspace.atomspace,
                                                deref(atom.handle))
    cdef cTruthValue* result_tv = result_tv_ptr.get()
    cdef strength_t strength = deref(result_tv).get_mean()
    cdef strength_t confidence = deref(result_tv).get_confidence()
    return TruthValue(strength, confidence)
