
## Preconditions

```nose``` Python testing framework is required.

Installation using ```apt-get``` package manager :
```
apt-get install python3-nose
```

## Running tests

You may need to set up the PYTHON path:
export PYTHONPATH=${PROJECT_BINARY_DIR}/opencog/cython
or, if installed:
export PYTHONPATH=/usr/local/lib/python3.7/dist-packages/opencog:${PYTHON}

For example:
export PYTHONPATH=build/opencog/cython PROJECT_SOURCE_DIR=.

Then from atomspace root source dir execute:

```
nosetests3 -vs ./tests/cython/
nosetests3 -vs ./tests/cython/atomspace/
nosetests3 -vs ./tests/cython/bindlink/
nosetests3 -vs ./tests/cython/guile/
nosetests3 -vs ./tests/cython/utilities/
```
