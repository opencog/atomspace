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
from opencog.type_ctors cimport get_frame


# basic wrapping for std::string conversion
cdef extern from "<string>" namespace "std":
    cdef cppclass string:
        string()
        string(char *)
        const char * c_str()
        int size()

cdef extern from "opencog/cython/opencog/PyScheme.h" namespace "opencog":
    string eval_scheme(cAtomSpace* as, const string& s) except +

def scheme_eval(a, str pys=None):
    """Evaluate Scheme expression and return string.
    Args:
        If one arg is supplied, then it must be the Scheme expression
        to evaluate.  It will be evaluated in the AtomSpace of the
        current thread.

        If two arguments are given, the first argument must be the
        AtomSpace to use during evaluation, and the second is the
        scheme expression. It will be evaluated in the supplied
        AtomSpace.
    Returns:
        str: result of the evaluation.
    Raises:
        RuntimeError: in case of evaluation error.
    See also:
        scheme_eval_v() for expressions that return Atoms or Values.
    """
    cdef string ret
    cdef string expr
    cdef cAtomSpace* asp
    cdef AtomSpace atomspace

    # Handle optional atomspace argument
    if pys is None:
        # First argument is the scheme string, use thread atomspace
        pys = a
        atomspace = AtomSpace_factoid(handle_cast(get_frame()))
    else:
        atomspace = a

    asp = <cAtomSpace*>atomspace.shared_ptr.get()
    expr = pys.encode('UTF-8')
    ret = eval_scheme(asp, expr)
    return ret.c_str().decode('UTF-8')

cdef extern from "opencog/cython/opencog/PyScheme.h" namespace "opencog":
    cValuePtr eval_scheme_v(cAtomSpace* as, const string& s) except +

def scheme_eval_v(a, str pys=None):
    """Evaluate Scheme expression, with expected result a Value or Atom.
    Args:
        If one arg is supplied, then it must be the Scheme expression
        to evaluate.  It will be evaluated in the AtomSpace of the
        current thread.

        If two arguments are given, the first argument must be the
        AtomSpace to use during evaluation, and the second is the
        scheme expression. It will be evaluated in the supplied
        AtomSpace.

        The evaluated expression must return a Value or an Atom;
        if it does not, an excption will be raised.
    Returns:
        Atom or Value, the result of the evaluation.
    Raises:
        RuntimeError: in case of evaluation error.
    See also:
        scheme_eval() for expressions that return strings.
    """
    cdef cValuePtr ret
    cdef string expr
    cdef cAtomSpace* asp
    cdef AtomSpace atomspace

    # Handle optional atomspace argument
    if pys is None:
        # First argument is the scheme string, use thread atomspace
        pys = a
        atomspace = AtomSpace_factoid(handle_cast(get_frame()))
    else:
        atomspace = a

    asp = <cAtomSpace*>atomspace.shared_ptr.get()
    expr = pys.encode('UTF-8')
    ret = eval_scheme_v(asp, expr)
    return create_python_value_from_c_value(ret)
