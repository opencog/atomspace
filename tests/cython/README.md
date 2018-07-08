
## Preconditions

```nose``` Python testing framework is required.

Installation using ```pip``` package manager :
```
pip install nose
```
or install it using ```conda``` package manager:
```
conda install nose
```

## Running tests

You may need to set up the PYTHON path:
export PYTHONPATH=${PROJECT_BINARY_DIR}/opencog/cython
or, if installed:
export PYTHONPATH=/usr/local/lib/python3.5/dist-packages/opencog:${PYTHON}

For example:
export PYTHONPATH=build/opencog/cython

Then from atomspace root source dir execute:

```
nosetests -vs ./tests/cython/
nosetests -vs ./tests/cython/atomspace/
nosetests -vs ./tests/cython/bindlink/
nosetests -vs ./tests/cython/guile/
nosetests -vs ./tests/cython/utilities/
```
