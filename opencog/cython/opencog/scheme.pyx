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
                                AtomSpace_factoid)


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
    expr = pys.encode('UTF-8')
    # print "Debug: called scheme eval with atomspace {0:x}".format(<unsigned long int>a.atomspace)
    ret = eval_scheme(a.atomspace, expr)
    return ret.c_str()

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
    expr = pys.encode('UTF-8')
    ret = eval_scheme_v(a.atomspace, expr)
    return Value.create(ret)

cdef extern from "opencog/cython/opencog/PyScheme.h" namespace "opencog":
    cHandle eval_scheme_h(cAtomSpace* as, const string& s) except +

def scheme_eval_h(AtomSpace a, str pys):
    """Evaluate Scheme program when expected result is Handle.
    Args:
        a (AtomSpace): atomspace to work on
        pys (str): Scheme program to evaluate
    Returns:
        Handle: result of a evaluation
    Raises:
        RuntimeError: in case of evaluation error
    """
    cdef cHandle ret
    cdef string expr
    expr = pys.encode('UTF-8')
    ret = eval_scheme_h(a.atomspace, expr)
    return Atom.createAtom(ret)

cdef extern from "opencog/cython/opencog/PyScheme.h" namespace "opencog":
    cValuePtr eval_scheme_as(const string& s) except +

def scheme_eval_as(str pys):
    """Evaluate Scheme program when expected result is AtomSpace.
    Args:
        pys (str): Scheme program to evaluate
    Returns:
        AtomSpace: atomspace result
    Raises:
        RuntimeError: if result of Scheme program is not AtomSpace or
            in case of evaluation error
    """
    cdef cValuePtr ret
    cdef string expr
    expr = pys.encode('UTF-8')
    ret = eval_scheme_as(expr)
    return AtomSpace_factoid(ret)

cdef extern from "opencog/cython/opencog/load-file.h" namespace "opencog":
    int load_scm_file_relative (cAtomSpace& as, char* filename) except +

def load_scm(AtomSpace a, str fname):
    fname_tmp = fname.encode('UTF-8')
    status = load_scm_file_relative(deref(a.atomspace), fname_tmp)
    return status == 0
