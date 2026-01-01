# Python Examples

## Intro Remarks
The examples below are written in python. From the point of view of
this projet (Atomese, the AtomSpace) programming in python (or scheme,
or even c++, for that matter) is an abuse. It is even wrong, in a sense.
The goal of this project is to provide infrastructure for sensori-motor
agents and organisms that can remember, create, move, analyze and react.
That infrastructure is "Atomese"; it is not python, scheme, c++ or any
other "human-oriented" programming language.  That is, Atomese is not
for "programming" but for "knowledge represetntation" in a machine-
readable, self-assembling, self-describing, self-reflecting manner.
Sensori-motor organisms are "made out of" Atomese.  Structures and
processes are embodied in Atomese, and are not to be glued together
in python, scheme or c++.

That said, there has to be a way to use Atomese "from the outside",
and python, scheme and c++ provide that interface, for now. These
allow Atomese components to be tinkered with. Thus, python provides
a way for constructing and manipulating Atomese.

Review the wiki page for more:
[Atomese](https://wiki.opencog.org/w/Atomese)

Documentation: ***Every*** Atom in the core AtomSpace has a wiki page
documenting it and how to use it. Some of the more important Atom and
Value types are:
* [Atom](https://wiki.opencog.org/w/Atom)
* [Link](https://wiki.opencog.org/w/Link)
* [Node](https://wiki.opencog.org/w/Node)
* [Value](https://wiki.opencog.org/w/Value)
* [FloatValue](https://wiki.opencog.org/w/FloatValue)
* [EdgeLink](https://wiki.opencog.org/w/EdgeLink)
* [VariableNode](https://wiki.opencog.org/w/VariableNode)
* [QueryLink](https://wiki.opencog.org/w/QueryLink)
* [RuleLink](https://wiki.opencog.org/w/RuleLink)
* [FilterLink](https://wiki.opencog.org/w/FilterLink)

## Tutorial
Start a python3 shell in your favorite way.

* From the python prompt, the following will list the currently
  installed OpenCog python modules:
  ```
      help('opencog')
  ```

* The main python module is `opencog.atomspace`. There are only two
  others: `opencog.logger` for the logging subsystem, and
  `opencog.scheme` for the python-to-scheme interfaces. It is not
  expected that there will ever be more (because the goal is to reduce
  the amount of python needed to interact with Atomese, and not to
  increase it.)  The contents of a module can be viewed by using `dir`
  function. For example:
  ```
      import opencog.atomspace
      print(dir(opencog.atomspace))
  ```

* You can run the examples from your shell. For example,
  ```
      python3 graph_edges.py
  ```

### [create_atoms.py](create_atoms.py)
The most basic example of creating Atoms in an AtomSpace, and attaching
weights to them.

### [graph_edges.py](graph_edges.py)
Another very basic example, showing the canonical form for representing
graph edges.

### [vector_tutorial.py](vector_tutorial.py)
A more complex example, showing how to perform queries, how to use the
query system to perform basic processing (counting, in this example) and
how to vectorize the results (so that vector data can be fed to GPU's.)

### [create_atoms_lowlevel.py](create_atoms_lowlevel.py)
For the bulk loading of an AtomSpace from python, direct access
to the AtomSpace is faster. The code is a little harder to read,
and exhibits less of the 'natural' Atomese syntax,

### [nameserver_example.py](nameserver_example.py)
The AtomSpace includes a nameserver and class factory. These allow
Atom Types to be directly accessed. This is an advanced demo; very
few users will need to work with the nameserver.

### [stop_go.py](stop_go.py)
An example of a "behavior tree". Demos the use of the
GroundedPredicateNode to call from Atomese back into python.

### Mixing python, scheme and Atomese
Atomese has both scheme and python bindings, and the two programming
languages and styles can be freely intermixed. That is, you can call
scheme from python, and python from scheme, and everything "just works".

Both python and scheme can be called from within Atomese, by using
the [GroundedSchemaNode](https://wiki.opencog.org/w/GroundedSchemaNode).
All of these languages can be intermixed, they are all bi-directional.

#### [scheme_sharing.py](scheme_sharing.py)
A basic example of sharing an AtomSpace between python and scheme.

#### [scheme_timer.py](scheme_timer.py)
Simple measurement of the performance overhead of invoking the scheme
(guile) evaluator.

#### ground
The [ground](ground) directory contains an example of calling python
from with Atomese.  The `stop_go.py` demo above is more clearly written
and simpler to understand.

## Python Virtualenv issues
Some users report unusual/unexpected issues when using Python VirtualEnv.
If you are experiencing issues with undefined symbols, then try adding
`/usr/local/lib/python3.11/dist-packages/` to your `PYTHON_PATH` and
adding `/usr/local/lib/opencog/` to your `LD_LIBRARY_PATH`.
