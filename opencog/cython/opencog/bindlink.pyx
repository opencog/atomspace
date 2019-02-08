from opencog.atomspace cimport Atom, AtomSpace
from opencog.atomspace cimport cHandle, cAtomSpace, cTruthValue
from opencog.atomspace cimport tv_ptr, strength_t, count_t
from cython.operator cimport dereference as deref
from opencog.type_constructors import TruthValue

def af_bindlink(AtomSpace atomspace, Atom atom):
    if atom == None: raise ValueError("af_bindlink atom is: None")
    cdef cHandle c_result = c_af_bindlink(atomspace.atomspace,
                                          deref(atom.handle))
    cdef Atom result = Atom.createAtom(c_result, atomspace)
    return result

def execute_atom(AtomSpace atomspace, Atom atom):
    if atom == None: raise ValueError("execute_atom atom is: None")
    cdef cHandle c_result = c_execute_atom(atomspace.atomspace,
                                           deref(atom.handle))
    return Atom.createAtom(c_result, atomspace)

def evaluate_atom(AtomSpace atomspace, Atom atom):
    if atom == None: raise ValueError("evaluate_atom atom is: None")
    cdef tv_ptr result_tv_ptr = c_evaluate_atom(atomspace.atomspace,
                                                deref(atom.handle))
    cdef cTruthValue* result_tv = result_tv_ptr.get()
    cdef strength_t strength = deref(result_tv).get_mean()
    cdef strength_t confidence = deref(result_tv).get_confidence()
    return TruthValue(strength, confidence)
