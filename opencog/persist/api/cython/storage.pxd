
from opencog.atomspace cimport cValuePtr, cHandle, cAtomSpace

cdef extern from "PersistCython.h" namespace "opencog":
# cdef extern from "PersistCython.h" :
	cdef void storage_open(const cHandle&)
