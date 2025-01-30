# Python Examples

The examples below are written in python. At the philosophical level,
this is just plain wrong. Atomese is not intended for human programmers,
it is intended for automated algorithms. This means that they syntax for
Atomese is very easy to parse, manipulate, process and rewrite: all
Atomese is just a (hyper-)graph. This also means that its verbose, and
often awkward for human programmers. You can do anything you want with
Atomese, but it is not a human-freindly programming language.

Roughly speaking, programming in Atomese is like programming in
assembly: it can be done, and many humans specialize in and enjoy
assembly coding. However, most of the rest stick to high-level
languages.  The analogy here, though is flawed: Atomese is for
machine manipulation; there isn't a high-level langauge built on
top of it, nor should there be. Conventional programming languages
already provide that. Atomese provides graphs. Use it as a graph.
That's what it's for,

That said: you can't debug a graph if you don't understand Atomese,
so all of the demo's below are written explicitly in Atomese. But
with a python-like-ish flavor.

* From the python prompt, the following should list the opencog python modules
  ```
      help('opencog')
  ```

* The contents of a single module can be viewed by using `dir` function.
  For example,
  ```
      import opencog.atomspace
      print(dir(opencog.atomspace))
  ```

* You can run the examples from your shell. For example,
  ```
      python3 storage_tutorial.py
  ```

## storage_tutorial.py
A relatively simple all-in-one tutorial introducing basic concepts,
a practical example, and the use of the store-to-disk backend.

## vector_tutorial.py
A more complex example, showing how to perform queries, how to use the
query system to perform basic processng (counting, in this example) and
how to vectorize the results (so that vector data an be fed to GPU's.)

## create_atoms_by_type.py, create_atoms_simple.py
Simple examples of how to create atoms in an atomspace.  These
demonstrate two different ways in which the API can be used; one
creates atoms according to their types, the other creates them
directly.

## get_outgoings.py
Simple example to show how to access the outgoing set of a link.

## stop_go.py
A simple example of a "behavior tree" implemented in
[Atomese](https://wiki.opencog.org/w/Atomese), the API for manipulating
AtomSpace contents.

## Scheme through python
Atomese has both scheme and python bindings, and the two programming
langauges and styles can be freely intermixed. That is, you can call
scheme from python, and python from scheme, and everything "just works".

The examples below show how this can be done.

### scheme/atom_type_names.py
Example of how to obtain atom type names and atom type IDs.

### scheme/bindlink.py
Example of how to use the pattern matcher BindLink functionality.

### scheme/scheme_timer.py
Simple measurement of the performance of invoking the scheme (guile)
interpreter.
