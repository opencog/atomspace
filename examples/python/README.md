# Python Examples

## Intro Remarks
The examples below are written in python. At the philosophical level,
this is just plain wrong. Atomese is not intended for human programmers,
it is intended for automated algorithms. This means that the syntax for
Atomese is was designed to make it easy for algorithms to create,
process and run it. The syntax is very easy to parse, manipulate
and rewrite: after all, Atomese is 'just' a (hyper-)graph.

This also means that it is verbose, and often awkward for human
programmers. You can express anything in Atomese, but it is not
a human-freindly programming language, and was never meant to be.

Roughly speaking, programming in Atomese is like programming in
assembly: it can be done, and many humans specialize in and enjoy
assembly coding. However, most of the rest stick to high-level
languages.  The analogy here, though, is flawed: Atomese is for
machine manipulation; there isn't a high-level language built on
top of it, nor should there be. Conventional programming languages
already do an excellent job. Atomese provides graphs. Use it as
a graph processing system.  That's what it's for.

Footnote: graph processing libraries, such as
[NetworkX](https://networkx.org/) are ***ideal*** for doing graph
processing, *if you are a human programmer*. And that's fine!  However,
if you are trying to create systems that automatically learn new
algorithms and structures, you need to be able to express your algo,
*as a graph*. And NetworkX cannot do that.

Consider the problem of creating a million algorithms, gving each one
of them a weight, or severalw weights. Maybe providing them with a vector
embedding, or perhaps vice-versa: the algos provide a basis for a vector
embedding of words. Word2Vec and GloVe attach vectors of numbers to
words. Imagine instead attaching vectors of algos to words. Now imagine
running them, all at the same time. Managing that, and adjusting
execution priority based on weights. Using them to process a shifting
stream of perceptual data. You can't do that with NetworkX. You can,
with Atomese. This is the raison d'etre.

All that said: if you are debugging complex graph processing algorithms,
you won't get far if you don't understand what the graph represents.
The demos in this directory walk through some of the basics of Atomese,
using python as the embedding language.

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

* The contents of a single module can be viewed by using `dir` function.
  For example,
  ```
      import opencog.atomspace
      import opencog.type_constructors
      print(dir(opencog.atomspace))
      print(dir(opencog.type_constructors))
  ```

* You can run the examples from your shell. For example,
  ```
      python3 storage_tutorial.py
  ```

### [create_atoms.py](create_atoms.py)
The most basic example of creating Atoms in an AtomSpace, and attaching
weights to them.

### [storage_tutorial.py](storage_tutorial.py)
A relatively simple all-in-one tutorial introducing basic concepts,
a practical example, and the use of the store-to-disk StorageNode API.

### [vector_tutorial.py](vector_tutorial.py)
A more complex example, showing how to perform queries, how to use the
query system to perform basic processng (counting, in this example) and
how to vectorize the results (so that vector data an be fed to GPU's.)

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
