
C++ to Guile wrapper examples
-----------------------------

The `PrimitiveExample.cc` file demonstrates how to wrap C++ class
methods with guile (scheme) wrappers. It also shows how to call guile
functions from C++ code, using the `SchemeEval` evaluator.

Compile the example with `make examples` in the `build` directory. The
resulting executable is in `build/examples/c++-guile/PrimitiveExample`.
Note: running the example prints a hefty stack trace, *on purpose*!
Do not be alarmed!

To make such functions accessible to scheme programs, they need to be
encapsulated in a guile module.  This is done in three parts:
* `ExampleSCM.cc` shows the C++ code, and how to wrap it into a module.
* `opencog/example.scm` shows the scheme boilerplate to load and
  initialize that module.
* `run-example.scm` shows how users can run the wrappers.
