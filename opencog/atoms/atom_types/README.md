Atom Type Hierarchy
===================
The `atom_types.script` file defines the type hierarchy of all of the
atoms defined in the AtomSpace.  By using the `NameServer`, one can
examine this type hierarchy at run-time.

This is not the only such file describing atom types: other systems
can add their own. For example, the natural language subsystem declares
a bunch of atom types for working with words and sentences. The agi-bio
subsystem defines a bunch of atom types for working with genes and
proteins.


QuickStart
==========
If just want to create some types and don't want to read the docs, then
go to

  https://github.com/opencog/cheminformatics/tree/master/cheminformatics/types

and copy the stuff there to your location, edit to suit your tastes,
and go. That's it.


Adding New Atom and Value Types
===============================
The `NameServer` provides an extension mechanism so that third parties
(i.e. other systems, libraries) may add new atom types to the default
AtomSpace type hierarchy.

To simplify the declaration and addition of new atom types, several
different CMake macros are provided.

Declaring a new Value or Atom type
----------------------------------
Atom type hierarchies are declared in an 'atom type script' file. It
uses the following format:
```
<TYPE> [<- <PARENT_TYPE1>[,<PARENT_TYPE2>,<PARENT_TYPE3>,...]] ["<TYPE_NAME>"]
```
Where

 * `TYPE` is an identifier that will be used in your code to reference
   the type's numeric code. Usually, it is defined using capital
   letters and underscores.

 * `PARENT_TYPE1, PARENT_TYPE1, PARENT_TYPE2` are optional identifiers of
   the parent types (supertypes) of the defined type. When more than one
   parent type is specified, they must be separated by commas.

 * `TYPE_NAME` is a string that will be used to identify the type. If
   none is supplied, the CMake macro will generate one based on the
   type's identifier using camel-casing patterns (for instance,
   `CUSTOM_NODE` would be named `CustomNode`).

Above is a short snippet of valid script entries. For more examples,
check the `atom_types.script` file in this directory.
```
ATOM
NODE <- ATOM
LINK <- ATOM
WORD_NODE <- NODE
CONCEPT_NODE <- NODE "OddlyNamedNode"
ASSOCIATIVE_LINK <- LINK "AssocL"
EVALUATION_LINK <- LINK "EvalLink"
MULTIPARENT_LINK <- ASSOCIATIVE_LINK_LINK,EVALUATION_LINK "MPLink"
```
-----

CMake and auto-generated files
-----------------------------
To process the `atom types` script file, one must add the macro
`OPENCOG_ADD_ATOM_TYPES` to the `CMakeLists.txt` and the header file
to the list of source files:

```
# CMakeList.txt
OPENCOG_ADD_ATOM_TYPES(atom_types.script atom_types.h atom_types.definitions atom_types.inheritance)

ADD_LIBRARY(sample
    atom_types.h
    Sample1.cc
    Sample2.cc
    ...
)
```
The macro `OPENCOG_ADD_ATOM_TYPES` expects six parameters:

1. The filename of the script file that will be used as input
2. The filename of the header file that will be generated with
   the identifiers of the new atom types.
3. The filename of the definitions file that will be generated with
   the instantiations of the variables that will store the new atom
   types.
4. The filename of the inheritance file that will be generated with
   the set of method invocations that will build the type hierarchy
   inside the NameServer.
5. The filename for the scheme bindings to the new atom types.
6. The filename for the python bindings to the new atom types.

Initializing the type hierarchy
------------------------------
The type hierarchy needs to be initialized before it is used. The
best way to do this is to define a shared library constructor, as
follows:

```
#include "atom_types.definitions"

static __attribute__ ((constructor)) void _init(void)
{
    opencog::nameserver().beginTypeDecls("my-custom-types");
    #include "atom_types.inheritance"
    opencog::nameserver().endTypeDecls();
}
```

See the `atom_types.cc` file for a full working example.

Using custom atom types
-----------------------
To use the newly-defined atom types, just include the type header file
in C++ code (similarly, the scheme module, of the python module).

For C++, assuming that a new `MY_NODE` was defined:
```
#include "atom_types.h"

using namespace opencog;

void some_function()
{
    Handle h = createNode(MY_NODE, "foo and bar");

    std::string tname = NameServer::getTypeName(MY_NODE);
}
```
