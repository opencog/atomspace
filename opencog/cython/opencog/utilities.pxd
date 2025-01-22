from libcpp.vector cimport vector
from libcpp.string cimport string
from libcpp.memory cimport shared_ptr
from libcpp.set cimport set as cpp_set
from opencog.atomspace cimport cAtomSpace, Type, tv_ptr, cHandle, cValuePtr


cdef extern from "opencog/cython/opencog/Utilities.h" namespace "opencog":
    cdef void c_initialize_python "opencog::initialize_python" ()
    cdef void c_finalize_python "opencog::finalize_python" ()
    cHandle c_add_node "opencog::add_node" (Type t, const string s) except +
    # cHandle c_add_node "opencog::add_node" (Type t, string s, tv_ptr tvn) except +
    cHandle c_add_link "opencog::add_link" (Type t, const vector[cHandle]) except +
    # cHandle c_add_link "opencog::add_link" (Type t, vector[cHandle], tv_ptr tvn) except +


cdef extern from "opencog/cython/executioncontext/Context.h" namespace "opencog":
    cValuePtr get_context_atomspace();
    void push_context_atomspace(cValuePtr atomspace);
    cValuePtr pop_context_atomspace();
    void c_clear_context "opencog::clear_context" ();


cdef extern from "opencog/atoms/core/FindUtils.h" namespace "opencog":
    bint c_is_closed "opencog::is_closed" (const cHandle& h)


cdef extern from "opencog/atoms/core/FindUtils.h" namespace "opencog":
    cpp_set[cHandle] c_get_free_variables "opencog::get_free_variables" (const cHandle& h)
