from libcpp.memory cimport shared_ptr
from cpython.object cimport PyObject
from libcpp cimport bool

cdef extern from "opencog/atoms/execution/GroundedObject.h" namespace "opencog":
    cdef cppclass cGroundedObject "opencog::GroundedObject":
        pass

cdef extern from "opencog/cython/opencog/PythonGroundedObject.h":
    cdef cppclass cPythonGroundedObject "opencog::PythonGroundedObject" (cGroundedObject):
        cPythonGroundedObject(void* object, bool unwrap_args)
        void* get_object() const

cdef extern from "opencog/atoms/execution/GroundedObjectNode.h" namespace "opencog":
    cdef cppclass cGroundedObjectNode "opencog::GroundedObjectNode":
        cGroundedObjectNode(const string&)
        cGroundedObjectNode(const string&, const shared_ptr[cGroundedObject]&)
        bool has_object() const
        void set_object(const shared_ptr[cGroundedObject]& object)
        cGroundedObject* get_object() except +

    ctypedef shared_ptr[cGroundedObjectNode] cGroundedObjectNodePtr "opencog::GroundedObjectNodePtr"

    cdef cGroundedObjectNodePtr cCreateGroundedObjectNode "createGroundedObjectNode" (...)

