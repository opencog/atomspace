from opencog.atomspace cimport AtomSpace
from opencog.type_constructors import set_type_ctor_atomspace

cdef extern from "Python.h":
    char *PyString_AsString(object)

# Avoid recursive intialization
is_initialized = False

def initialize_opencog(AtomSpace atomspace, object config = None):

    # Avoid recursive intialization
    global is_initialized
    if is_initialized:
        set_type_ctor_atomspace(atomspace)
        return
    is_initialized = True

    cdef char *configFileString
    if (config == None):
        configFileString = NULL
    else:
        configFileString = PyString_AsString(config)
    c_initialize_opencog(atomspace.atomspace, configFileString)
    set_type_ctor_atomspace(atomspace)

def finalize_opencog():
    global is_initialized
    if is_initialized:
        c_finalize_opencog()
        
    set_type_ctor_atomspace(None)

def configuration_load(object config):
    cdef char *configFileString = PyString_AsString(config)
    c_configuration_load(configFileString)
