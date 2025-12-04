from libcpp.vector cimport vector
from libcpp.string cimport string
from libcpp.memory cimport shared_ptr
from libcpp.set cimport set as cpp_set
from opencog.atomspace cimport cAtomSpace, Type, cHandle, cValuePtr

cdef extern from "opencog/util/exceptions.h" namespace "opencog":
    cdef cppclass cPythonException "opencog::PythonException":
        const string& get_python_exception_type() except +
        const char* get_message() except +


cdef extern from "opencog/cython/opencog/TypeCtors.h" namespace "opencog":
    cHandle c_add_node "opencog::add_node" (Type t, const string s) nogil except +
    cHandle c_add_link "opencog::add_link" (Type t, const vector[cHandle]) nogil except +

cdef extern from "opencog/eval/FrameStack.h" namespace "opencog":
    cValuePtr get_frame() nogil
    void push_frame(cHandle atomspace) nogil
    cValuePtr pop_frame() nogil
    void set_frame(cHandle atomspace) nogil
    void clear_frame_stack() nogil
