# Python Examples


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
