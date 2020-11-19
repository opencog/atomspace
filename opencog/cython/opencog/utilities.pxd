from libcpp.vector cimport vector
from libcpp.string cimport string
from libcpp.memory cimport shared_ptr
from opencog.atomspace cimport cAtomSpace, Type, tv_ptr, cHandle


cdef extern from "opencog/cython/opencog/Utilities.h" namespace "opencog":
    cdef void c_initialize_python "opencog::initialize_python" ()
    cdef void c_finalize_python "opencog::finalize_python" ()
    cHandle c_add_node "opencog::add_node" (Type t, const string s) except +
    # cHandle c_add_node "opencog::add_node" (Type t, string s, tv_ptr tvn) except +
    cHandle c_add_link "opencog::add_link" (Type t, const vector[cHandle]) except +
    # cHandle c_add_link "opencog::add_link" (Type t, vector[cHandle], tv_ptr tvn) except +


cdef extern from "opencog/cython/executioncontext/Context.h" namespace "opencog":
    cAtomSpace * get_context_atomspace();
    void push_context_atomspace(cAtomSpace * atomspace);
    cAtomSpace * pop_context_atomspace();
    void c_clear_context "opencog::clear_context" ();


# Its located in "opencog/persist/sexpr/" in the source tree,
# but is installed to "opencog/persist/file/". Cython wants the
# source location, not the install location.
cdef extern from "opencog/persist/sexpr/fast_load.h" namespace "opencog":
    void c_load_file "opencog::load_file" (const string path, cAtomSpace & atomspace);

cdef extern from "opencog/atoms/core/FindUtils.h" namespace "opencog":
    bint c_is_closed "opencog::is_closed" (const cHandle& h)
