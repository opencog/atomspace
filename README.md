OpenCog AtomSpace
=================

[![Build Status](
https://circleci.com/gh/opencog/atomspace/tree/master.png?circle-token=:circle-token)](https://circleci.com/gh/opencog/atomspace/tree/master)

The OpenCog Atomspace is the hypergraph database and query/reasoning
engine used by OpenCog to hold data and perform reasoning on it.

The main project site is at http://opencog.org

An interactive tutorial for getting started is available at:
https://github.com/opencog/opencog/blob/master/TUTORIAL.md

Prerequisites
-------------
To build the OpenCog AtomSpace, the packages listed below are required.
With a few exceptions, most Linux distributions will provide these
packages. Users of Ubuntu 14.04 "Trusty Tahr" may use the dependency
installer at `/scripts/octool`.  Users of any version of Linux may
use the Dockerfile to quickly build a container in which OpenCog will
be built and run.

###### boost
> C++ utilities package
> http://www.boost.org/ | libboost-dev

###### cmake
> Build management tool; v2.8 or higher recommended.
> http://www.cmake.org/ | cmake

###### cogutil
> Common OpenCog C++ utilities
> http://github.com/opencog/cogutils
> It uses exactly the same build procedure as this package. Be sure
  to `sudo make install` at the end.

###### cxxtest
> Test framework
> http://cxxtest.sourceforge.net/ | https://launchpad.net/~opencog-dev/+archive/ppa

###### guile
> Embedded scheme interpreter (version 2.0.9 or newer is required)
> http://www.gnu.org/software/guile/guile.html | guile-2.0-dev

###### libgsl
> The GNU Scientific Library
> http://www.gnu.org/software/gsl/ | libgsl0-dev

Optional Prerequisites
----------------------
The following packages are optional. If they are not installed, some
optional parts of OpenCog will not be built.  The CMake command, during
the build, will be more precise as to which parts will not be built.

###### HyperTable
> Distributed storage
> http://hypertable.org
> This requires SIGAR as well

###### Threading Building Blocks
> C++ template library for parallel programming
> https://www.threadingbuildingblocks.org/download | libtbb-dev

###### unixODBC
> Generic SQL Database client access libraries
> Required for the distributed-processing atomspace.
> http://www.unixodbc.org/ | unixodbc-dev

###### ZeroMQ (version 3.2.4 or higher)
> Asynchronous messaging library
> http://zeromq.org/intro:get-the-software | libzmq3-dev

Building AtomSpace
------------------
Perform the following steps at the shell prompt:
```
    cd to project root dir
    mkdir build
    cd build
    cmake ..
    make
```
Libraries will be built into subdirectories within build, mirroring
the structure of the source directory root.


Unit tests
----------
To build and run the unit tests, from the `./build` directory enter
(after building opencog as above):
```
    make test
```

Install
-------
After building, you MUST install the atomspace.
```
    sudo make install
```

Using the AtomSpace
-------------------
The AtomSpace can be used in one of three ways, or a mixture of all three:
By using the GNU Guile scheme interface, by using Python, or by running
the OpenCog cogserver.

Guile provides the easiest interface for creating atoms, loading them
into the AtomSpace, and performing various processing operations on
them.  For examples, see the `/examples/guile` and the
`/examples/pattern-matcher` directories.

Python is more familiar than scheme (guile) to most programmers, and
it offers another way of intrfacing to the atomspace. See the
`/examples/python` directory for how to use python with the AtomSpace.

The OpenCog cogserver provides a network server interface to OpenCog.
It is required for running embodiment, some of the reasoning agents,
and some of the natural-language processing agents.  The cogserver is
only available in the main OpenCog project; it is not a part of the
AtomSpace.

CMake notes
-----------
Some useful CMake's web sites/pages:

 - http://www.cmake.org (main page)
 - http://www.cmake.org/Wiki/CMake_Useful_Variables
 - http://www.cmake.org/Wiki/CMake_Useful_Variables/Get_Variables_From_CMake_Dashboards
 - http://www.cmake.org/Wiki/CMakeMacroAddCxxTest
 - http://www.cmake.org/Wiki/CMake_HowToFindInstalledSoftware


The main CMakeLists.txt currently sets -DNDEBUG. This disables Boost
matrix/vector debugging code and safety checks, with the benefit of
making it much faster. Boost sparse matrixes and (dense) vectors are
currently used by ECAN's ImportanceDiffusionAgent. If you use Boost
ublas in other code, it may be a good idea to at least temporarily
unset NDEBUG. Also if the Boost assert.h is used it will be necessary
to unset NDEBUG. Boost ublas is intended to respond to a specific
BOOST_UBLAS_NDEBUG, however this is not available as of the current
Ubuntu standard version (1.34).

-Wno-deprecated is currently enabled by default to avoid a number of
warnings regarding hash_map being deprecated (because the alternative
is still experimental!)
