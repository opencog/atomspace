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

cdef api cValuePtr call_python_method(object obj, const string& method_name,
                                      cAtomSpace* atomspace, const cValuePtr&
                                      _args):
    method = getattr(obj, method_name.c_str().decode())
    args = create_value_by_type(_args.get().get_type(),
                                PtrHolder.create(<shared_ptr[void]&>_args),
                                AtomSpace_factory(atomspace))
    cdef Value result = method(args)
    return result.get_c_value_ptr()

