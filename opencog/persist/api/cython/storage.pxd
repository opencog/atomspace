
from libcpp cimport bool
from libcpp.string cimport string
from opencog.atomspace cimport cAtomSpace

cdef extern from "opencog/persist/file/fast_load.h" namespace "opencog":
	void c_load_file "opencog::load_file" (const string path, cAtomSpace & atomspace);

# ----------------------------------------------------------------
