
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
Steps:
* Get some OpenCL GPU hardware, such as a Radeon graphics card.
* Install `clinfo` and `mesa-opencl-icd` and `opencl-headers`

Make sure the software isn't insane, by running
`opencog/opencl/scaffolding/show-ocl-hw` executable from the `build`
directory. It will print a short-form hardware listing that should
match what the `clinfo` command lists. If it doesn't, something is
wrong with the code here.

Make sure you can talk to the hardware, by running the
`opencog/opencl/scaffolding/run-hello-world` executable from the `build`
directory. It should print `>>This is only a test<<` if the code ran
on the GPUs.  It will work only if there is a copy of `hello.cl` in
whatever directory that you are running `run-hello-world` from.


General reading
---------------
...
