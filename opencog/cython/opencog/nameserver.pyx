#
# nameserver.pyx
#

# from atomspace cimport cNameServer, nameserver, NOTYPE, Type
from libc.string cimport strcmp
from libcpp cimport string
import sys
import warnings
from contextlib import contextmanager

# Dynamically construct a "types" module.
# XXX FIXME This should also listen to "addtype" signals in case
# new types are added dynamically (which they are, when new cogserver
# modules are loaded.)

# Given a numeric type, look up the string name.
cdef c_get_type_name(Type t):
    # cdef cNameServer ns
    # ns = nameserver()
    cdef string s
    s = nameserver().getTypeName(t)

    if 0 == strcmp(s.c_str(), "*** Unknown Type! ***") :
        s = string(b"")
    return s.c_str()

# Given the string name, look up the numeric type.
cdef c_get_named_type(str type_name):
    return nameserver().getType(type_name.encode('UTF-8'))

# Atom type methods.
def get_type_name(t):
    return c_get_type_name(t).decode('UTF-8')

def get_type(name):
    return c_get_named_type(name)

def is_a(Type t1, Type t2):
    return nameserver().isA(t1,t2)

# We seem to want to do atom types in two different styles. In one style,
# typenames are just strings, and we build a ordinary python dictionary
# so that we can get an integer ID for that name-string.
#
# In the other style, we have a python class, whose attributes have
# names that correspond to the actual Atomese atom type. This is much
# stricter than just using strings, because if you have a typo in the
# name, python will explcitly tell you about that. This does make things
# much easier, debugging-wise, as that way you don't have to stare at
# broken code without realizing its broken because of some minor typo.
#
# Dynamically building a python class with a bunch of attributes on it:
#
# From Roger's suggestion: (who's Roger?)
#import sys
#mod = sys.modules[__name__]
#
#for name in ['A', 'B', 'C']:
#    class_ = type(name, (object, ), {})
#    setattr(mod, name, class_)

# Create a python dictionary holding the string-name -> id number lookup.
typedict = {}
cdef generate_typedict():
    global typedict
    typedict = {}
    cdef string s
    # print("Class server has num types=", nameserver().getNumberOfClasses())
    for i in range(0, nameserver().getNumberOfClasses()):
        s = nameserver().getTypeName(i)
        assert s.size() > 0, "Got blank type name while generating types module"
        typedict[string(s.c_str()).decode('UTF-8')] = i
        # print("type ", i, " has name ", string(s.c_str()).decode('UTF-8'))
    typedict["NO_TYPE"] = NOTYPE
    return typedict

# Bulk-define the types class
types = type('atom_types', (), generate_typedict())

# Update/refresh list of types. This needs to be called whenever
# additional atom types were declared in other atomspace modules.
# i.e. when new types were added to the C++ nameserver.
def regenerate_types():
    global typedict
    global types
    # print("Enter regenerate_types")
    # A one-liner solution would have been this:
    # types = type('atom_types', (), generate_typedict())
    # but the above one-liner doesn't work. So just reiterate
    # over the typedict, one line at a time.
    generate_typedict()
    for t in typedict:
        # setattr(types, name, type_id)
        setattr(types, t, typedict[t])
    # print("Exit regenerate_types")
    return types

# Provide API so that new atom types can be added with python.
def begin_type_decls(module):
    return nameserver().beginTypeDecls(module.encode('UTF-8'))

def end_type_decls():
    nameserver().endTypeDecls()

@contextmanager
def type_decl_context(module):
    if begin_type_decls(module):
        raise RuntimeError('Cannot declare types for already loaded module: ' +
                module)
    try:
        yield
    finally:
        end_type_decls()

def decl_type(parent, name):
    type_id = nameserver().declType(parent, name.encode('UTF-8'))
    setattr(types, name, type_id)
    return type_id

# Cache for Python constructors
cdef dict _python_ctor_cache = {}

cdef create_python_value_from_c_value(const cValuePtr& value):
    if value.get() == NULL:
        return None

    cdef cValue* val_ptr = value.get()
    cdef Type value_type = val_ptr.get_type()
    cdef str type_name
    cdef Value instance

    # Check cache first
    cdef object py_class_ctor = _python_ctor_cache.get(value_type)
    if py_class_ctor is not None:
        instance = py_class_ctor.__new__(py_class_ctor)
        instance.shared_ptr = value
        return instance

    # Cache miss - find the correct python ctor to use.
    # AtomSpace must be checked before Atom since AtomSpace is-a Atom
    if is_a(value_type, types.AtomSpace):
        py_class_ctor = AtomSpace
    elif is_a(value_type, types.Atom):
        py_class_ctor = Atom
    elif is_a(value_type, types.QueueValue):
        py_class_ctor = QueueValue
    elif is_a(value_type, types.UnisetValue):
        py_class_ctor = UnisetValue
    elif is_a(value_type, types.LinkValue):
        py_class_ctor = LinkValue
    elif is_a(value_type, types.FloatValue):
        py_class_ctor = FloatValue
    elif is_a(value_type, types.StringValue):
        py_class_ctor = StringValue
    elif is_a(value_type, types.BoolValue):
        py_class_ctor = BoolValue
    elif is_a(value_type, types.VoidValue):
        py_class_ctor = VoidValue
    else:
        # Can't build one of these - this should never happen
        type_name = get_type_name(value_type)
        raise TypeError("Non-constructible Type: " + type_name)

    # Cache the result (is_a() results never change)
    _python_ctor_cache[value_type] = py_class_ctor
    instance = py_class_ctor.__new__(py_class_ctor)
    instance.shared_ptr = value
    return instance

# ========================== END OF FILE =========================
