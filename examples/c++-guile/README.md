
C++ to Guile wrapper examples
-----------------------------

The PrimitiveExample.cc file demonstrates how a C++ class can be
wrapped by guile, so that the C++ methods become callable in guile.

It is best if such wrappers are encapsulated in a guile module.
The ExampleSCM.cc file demonstrates how to create a new guile
module; the opencog/example.scm file shows how to set up that module.
