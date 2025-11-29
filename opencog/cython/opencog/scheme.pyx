"""
Scheme wrapper for Python

Before using the scheme, you will need to import any atom type
definitions that you require. For a detailed example of how to use this
functionality, see:

    tests/cython/guile/test_pattern.py

Also refer to the list of .scm type definition files in opencog.conf
"""

from cython.operator cimport dereference as deref
from opencog.atomspace cimport (cValuePtr, Value, cAtomSpace,
                                Atom, AtomSpace, cAtom, cHandle,
                                AtomSpace_factoid, handle_cast,
                                create_python_value_from_c_value)


# basic wrapping for std::string conversion
cdef extern from "<string>" namespace "std":
    cdef cppclass string:
        string()
        string(char *)
        const char * c_str()
        int size()

cdef extern from "opencog/cython/opencog/PyScheme.h" namespace "opencog":
    string eval_scheme(cAtomSpace* as, const string& s) except +

def scheme_eval(AtomSpace a, str pys):
    """Evaluate Scheme program and return string.
    Args:
        a (AtomSpace): atomspace to work on
        pys (str): Scheme program to evaluate
    Returns:
        str: result of a evaluation
    Raises:
        RuntimeError: in case of evaluation error
    """
    cdef string ret
    cdef string expr
    cdef cAtomSpace* asp = <cAtomSpace*>a.shared_ptr.get()
    expr = pys.encode('UTF-8')
    ret = eval_scheme(asp, expr)
    return ret.c_str().decode('UTF-8')

cdef extern from "opencog/cython/opencog/PyScheme.h" namespace "opencog":
    cValuePtr eval_scheme_v(cAtomSpace* as, const string& s) except +

def scheme_eval_v(AtomSpace a, str pys):
    """Evaluate Scheme program when expected result is Value.
    Args:
        a (AtomSpace): atomspace to work on
        pys (str): Scheme program to evaluate
    Returns:
        Value: result of a evaluation
    Raises:
        RuntimeError: in case of evaluation error
    """
    cdef cValuePtr ret
    cdef string expr
    cdef cAtomSpace* asp = <cAtomSpace*>a.shared_ptr.get()
    expr = pys.encode('UTF-8')
    ret = eval_scheme_v(asp, expr)
    return create_python_value_from_c_value(ret)
