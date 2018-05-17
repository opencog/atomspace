from atomspace cimport cNameServer, nameserver, NOTYPE, string, Type
from nameserver cimport strcmp


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
        s = string("")
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

# From Roger's suggestion:
#import sys
#mod = sys.modules[__name__]
#
#for name in ['A', 'B', 'C']:
#    class_ = type(name, (object, ), {})
#    setattr(mod, name, class_)

types = {}
cdef generate_type_module():
    global types
    types = {}
    cdef string s
    # print "Class server has num types=", nameserver().getNumberOfClasses()
    for i in range(0, nameserver().getNumberOfClasses()):
        s = nameserver().getTypeName(i)
        assert s.size() > 0, "Got blank type name while generating types module"
        types[string(s.c_str()).decode('UTF-8')] = i
        # print "type ", i, " has name ", s
    types["NO_TYPE"] = NOTYPE
    return types

types = type('atom_types', (), generate_type_module())

#This function is for refreshing new types
#ex. When you import a cython module which include non-core atom types in C++
#And you can refresh these new types by this function

def get_refreshed_types():
    global types
    types = type('atom_types', (), generate_type_module())
    return types
