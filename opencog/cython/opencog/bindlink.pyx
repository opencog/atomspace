from opencog.atomspace cimport Atom, AtomSpace, TruthValue
from opencog.atomspace cimport cHandle, cAtomSpace, cTruthValue
from opencog.atomspace cimport tv_ptr, strength_t, count_t
from cython.operator cimport dereference as deref


def stub_bindlink(AtomSpace atomspace, Atom atom):
    if atom == None: raise ValueError("stub_bindlink atom is: None")
    cdef cHandle c_result = c_stub_bindlink(atomspace.atomspace,
                                            deref(atom.handle))
    cdef Atom result = Atom(c_result.value(), atomspace)
    return result

def bindlink(AtomSpace atomspace, Atom atom):
    if atom == None: raise ValueError("bindlink atom is: None")
    cdef cHandle c_result = c_bindlink(atomspace.atomspace,
                                       deref(atom.handle), -1)
    cdef Atom result = Atom(c_result.value(), atomspace)
    return result

def single_bindlink(AtomSpace atomspace, Atom atom):
    if atom == None: raise ValueError("single_bindlink atom is: None")
    cdef cHandle c_result = c_bindlink(atomspace.atomspace,
                                       deref(atom.handle), 1)
    cdef Atom result = Atom(c_result.value(), atomspace)
    return result

def first_n_bindlink(AtomSpace atomspace, Atom atom, max_results):
    if atom == None: raise ValueError("first_n_bindlink atom is: None")
    if not isinstance(max_results, int):
        raise ValueError("first_n_bindlink max_results is not integer")
    cdef cHandle c_result = c_bindlink(atomspace.atomspace,
                                       deref(atom.handle), max_results)
    cdef Atom result = Atom(c_result.value(), atomspace)
    return result

def af_bindlink(AtomSpace atomspace, Atom atom):
    if atom == None: raise ValueError("af_bindlink atom is: None")
    cdef cHandle c_result = c_af_bindlink(atomspace.atomspace, 
                                          deref(atom.handle))
    cdef Atom result = Atom(c_result.value(), atomspace)
    return result

def satisfaction_link(AtomSpace atomspace, Atom atom):
    if atom == None: raise ValueError("satisfaction_link atom is: None")
    cdef tv_ptr result_tv_ptr = c_satisfaction_link(atomspace.atomspace,
                                                 deref(atom.handle))
    cdef cTruthValue* result_tv = result_tv_ptr.get()
    cdef strength_t strength = deref(result_tv).getMean()
    cdef strength_t confidence = deref(result_tv).getConfidence()
    return TruthValue(strength, confidence)

def satisfying_set(AtomSpace atomspace, Atom atom):
    if atom == None: raise ValueError("satisfying_set atom is: None")
    cdef cHandle c_result = c_satisfying_set(atomspace.atomspace,
                                             deref(atom.handle), -1)
    cdef Atom result = Atom(c_result.value(), atomspace)
    return result

def satisfying_element(AtomSpace atomspace, Atom atom):
    if atom == None: raise ValueError("satisfying_element atom is: None")
    cdef cHandle c_result = c_satisfying_set(atomspace.atomspace,
                                             deref(atom.handle), 1)
    cdef Atom result = Atom(c_result.value(), atomspace)
    return result

def first_n_satisfying_set(AtomSpace atomspace, Atom atom, max_results):
    if atom == None: raise ValueError("first_n_satisfying_set atom is: None")
    if not isinstance(max_results, int):
        raise ValueError("first_n_satisfying_set max_results is not integer")
    cdef cHandle c_result = c_satisfying_set(atomspace.atomspace,
                                             deref(atom.handle), max_results)
    cdef Atom result = Atom(c_result.value(), atomspace)
    return result

def execute_atom(AtomSpace atomspace, Atom atom):
    if atom == None: raise ValueError("execute_atom atom is: None")
    cdef cHandle c_result = c_execute_atom(atomspace.atomspace,
                                           deref(atom.handle))
    return Atom(c_result.value(), atomspace)

def evaluate_atom(AtomSpace atomspace, Atom atom):
    if atom == None: raise ValueError("evaluate_atom atom is: None")
    cdef tv_ptr result_tv_ptr = c_evaluate_atom(atomspace.atomspace,
                                                deref(atom.handle))
    cdef cTruthValue* result_tv = result_tv_ptr.get()
    cdef strength_t strength = deref(result_tv).getMean()
    cdef strength_t confidence = deref(result_tv).getConfidence()
    return TruthValue(strength, confidence)
