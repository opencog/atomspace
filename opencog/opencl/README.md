
OpenCL Scaffolding
==================
Experimental effort to target OpenCL

Version 0.0.2.

Overview
--------
Directory layout:

* The [scaffolding](scaffolding) directory contains some bring-up code.
* The [opencl-types](opencl-types) directory contains defintions for
  some OpenCL Atom types.
* The [stream](stream) directory contains implementations for those
  Atom types.


HOWTO
-----
Run the `opencog/opencl/scaffolding/scaffolding` executable from the
`build` directory. It will print a short-form hardware listing that
should match what the `clinfo` command lists.

Make sure that there is a copy of `hello.cl` in whatever directory
you are running `scaffolding` from.


General reading
---------------
...
