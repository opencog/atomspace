
HOWTO run these tests by hand:
------------------------------

You need to set up the PYTHON path:
export PYTHONPATH=${PROJECT_BINARY_DIR}/opencog/cython
or, if installed:
export PYTHONPATH=/usr/local/share/opencog/python

For example:
export PYTHONPATH=build/opencog/cython

Then:

nosetests -vs ./tests/cython/
nosetests -vs ./tests/cython/atomspace/
nosetests -vs ./tests/cython/bindlink/
nosetests -vs ./tests/cython/guile/
nosetests -vs ./tests/cython/utilities/


If you modify the cython bindings, you may need to manually remove
some build files to get a clean rebuild.  Basically, the CMakefiles
for cython/python are buggy, and fail to rebuild when changes are made.
So:

rm build/opencog/cython/opencog/pymoses.cpp
