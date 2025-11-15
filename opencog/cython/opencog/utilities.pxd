from libcpp.vector cimport vector
from libcpp.string cimport string
from libcpp.memory cimport shared_ptr
from libcpp.set cimport set as cpp_set
from opencog.atomspace cimport cAtomSpace, Type, cHandle, cValuePtr


cdef extern from "opencog/util/exceptions.h" namespace "opencog":
    cdef cppclass cPythonException "opencog::PythonException":
        const string& get_python_exception_type() except +
        const char* get_message() except +


cdef extern from "opencog/cython/opencog/Utilities.h" namespace "opencog":
    cHandle c_add_node "opencog::add_node" (Type t, const string s) except +
    cHandle c_add_link "opencog::add_link" (Type t, const vector[cHandle]) except +

cdef extern from "opencog/cython/executioncontext/Context.h" namespace "opencog":
    cValuePtr get_context_atomspace();
    void push_context_atomspace(cValuePtr atomspace);
    cValuePtr pop_context_atomspace();
    void c_clear_context "opencog::clear_context" ();


cdef extern from "opencog/atoms/core/FindUtils.h" namespace "opencog":
    bint c_is_closed "opencog::is_closed" (const cHandle& h)


cdef extern from "opencog/atoms/core/FindUtils.h" namespace "opencog":
    cpp_set[cHandle] c_get_free_variables "opencog::get_free_variables" (const cHandle& h)
