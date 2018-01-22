"""
Scheme wrapper for Python

Before using the scheme_wrapper, you will need to import any atom type
definitions that you require. For a detailed example of how to use this
functionality, see:

    tests/cython/guile/test_pattern.py

Also refer to the list of .scm type definition files in opencog.conf
"""

from cython.operator cimport dereference as deref
from opencog.atomspace cimport cAtomSpace, Atom, AtomSpace, cAtom, cHandle, AtomSpace_factory, void_from_candle


# basic wrapping for std::string conversion
cdef extern from "<string>" namespace "std":
    cdef cppclass string:
        string()
        string(char *)
        char * c_str()
        int size()

cdef extern from "opencog/cython/opencog/PyScheme.h" namespace "opencog":
    string eval_scheme(cAtomSpace* as, const string& s) except +

def scheme_eval(AtomSpace a, str pys):
    """
    Returns a string value
    """
    cdef string ret
    cdef string expr
    expr = pys.encode('UTF-8')
    # print "Debug: called scheme eval with atomspace {0:x}".format(<unsigned long int>a.atomspace)
    ret = eval_scheme(a.atomspace, expr)
    return ret.c_str()

cdef extern from "opencog/cython/opencog/PyScheme.h" namespace "opencog":
    cHandle eval_scheme_h(cAtomSpace* as, const string& s) except +

def scheme_eval_h(AtomSpace a, str pys):
    """
    Returns a Handle
    """
    cdef cHandle ret
    cdef string expr
    expr = pys.encode('UTF-8')
    ret = eval_scheme_h(a.atomspace, expr)
    return Atom(void_from_candle(ret), a)

cdef extern from "opencog/cython/opencog/PyScheme.h" namespace "opencog":
    cAtomSpace* eval_scheme_as(const string& s) except +

def scheme_eval_as(str pys):
    """
    Returns an AtomSpace
    """
    cdef cAtomSpace* ret
    cdef string expr
    expr = pys.encode('UTF-8')
    ret = eval_scheme_as(expr)
    return AtomSpace_factory(ret)

cdef extern from "opencog/cython/load-file.h" namespace "opencog":
    int load_scm_file_relative (cAtomSpace& as, char* filename) except +

def load_scm(AtomSpace a, str fname):
    fname_tmp = fname.encode('UTF-8')
    status = load_scm_file_relative(deref(a.atomspace), fname_tmp)
    return status == 0
