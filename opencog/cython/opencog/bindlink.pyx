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
