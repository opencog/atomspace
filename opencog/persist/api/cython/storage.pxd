
from libcpp cimport bool
from opencog.atomspace cimport cValuePtr, cHandle, cAtomSpace

cdef extern from "PersistCython.h" namespace "opencog":
# cdef extern from "PersistCython.h" :
	cdef void storage_open(const cHandle&)
	cdef void storage_close(const cHandle&)
	cdef bool storage_connected(const cHandle&)

	cdef cHandle dflt_fetch_atom(const cHandle&)
	cdef cHandle dflt_store_atom(const cHandle&)
