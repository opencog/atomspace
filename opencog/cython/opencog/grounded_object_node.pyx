from opencog.atomspace cimport (cAtom, cCreateGroundedObjectNode,
                                cPythonGroundedObject)
from libcpp.memory cimport shared_ptr
from libcpp.string cimport string
from cython.operator cimport dereference as deref

def createGroundedObjectNode(name, obj, atomspace):
    cdef shared_ptr[cGroundedObject] o_ptr
    cdef shared_ptr[cGroundedObjectNode] node_ptr
    cdef string node_name = <bytes>(name.encode())
    if obj is not None:
        o_ptr.reset(new cPythonGroundedObject(<PyObject*>obj))
        node_ptr.reset(new cGroundedObjectNode(node_name, o_ptr))
    else:
        node_ptr.reset(new cGroundedObjectNode(node_name))

    return GroundedObjectNode(PtrHolder.create(<shared_ptr[void]&>node_ptr),
                              atomspace)

cdef class GroundedObjectNode(Atom):

    def __init__(self, ptr_holder, atomspace):
        super(GroundedObjectNode, self).__init__(ptr_holder, atomspace)

    cdef cGroundedObjectNode* get_c_grounded_object_node_ptr(self):
        return <cGroundedObjectNode*>(self.get_c_value_ptr().get())

    def set_object(self, obj):
        cdef shared_ptr[cGroundedObject] o_ptr
        o_ptr.reset(new cPythonGroundedObject(<PyObject*>obj))
        self.get_c_grounded_object_node_ptr().set_object(o_ptr)

    def get_object(self):
        cdef cGroundedObjectNode* gon = self.get_c_grounded_object_node_ptr()
        if not gon.has_object():
            return None
        cdef cPythonGroundedObject* py_gon = <cPythonGroundedObject*>gon.get_object()
        return py_gon.get_object()

cdef api cValuePtr call_python_method(object obj, const string& method_name,
                                      cAtomSpace* atomspace, const cValuePtr&
                                      _args):
    method = getattr(obj, method_name.c_str().decode())
    assert _args.get().is_link()
    args = convert_handle_seq_to_python_list(
        (<cAtom*>_args.get()).getOutgoingSet(), AtomSpace_factory(atomspace))
    cdef Value result = method(*args)
    return result.get_c_value_ptr()

