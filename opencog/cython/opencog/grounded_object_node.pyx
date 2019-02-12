from opencog.atomspace cimport cCreateGroundedObjectNode, cPythonGroundedObject
from libcpp.memory cimport shared_ptr
from libcpp.string cimport string
from cython.operator cimport dereference as deref

def createGroundedObjectNode(name, obj, atomspace):
    cdef shared_ptr[cPythonGroundedObject] o_ptr
    o_ptr.reset(new cPythonGroundedObject(<PyObject*>obj))
    node_ptr = cCreateGroundedObjectNode(<bytes>(name.encode()), o_ptr)

    return GroundedObjectNode(PtrHolder.create(<shared_ptr[void]&>node_ptr),
                              atomspace)

cdef class GroundedObjectNode(Atom):

    def __init__(self, ptr_holder, atomspace):
        super(GroundedObjectNode, self).__init__(ptr_holder, atomspace)

