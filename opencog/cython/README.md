Python bindings for OpenCog
---------------------------

## Requirements ##

* Python3, any version
* Cython 0.14 or later. http://www.cython.org/
* Nosetests - for running unit tests.

Both Cython and Nosetests can be installed with `apt-get`:
```
    sudo apt-get install cython python3-nose
```
or with `easy_install`:
```
    sudo easy_install cython nose3
```
The bindings are written mostly using Cython, which allows writing
code that looks pythonic but gets compiled to C.

Currently the package structure looks like this:
```
    opencog.atomspace
    opencog.atomspace.types
    opencog.scheme
```
## Tutorial ##

The OpenCog wiki contains the Python tutorial:

http://wiki.opencog.org/w/Python
