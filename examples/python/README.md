# Python Examples

It is assumed Python 3 is the default version on your system. If you
use Python 2 you will have to modify the commands and scripts
accordingly.

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
      python create_atoms_by_type.py
  ```
  
## create_atoms_by_type.py, create_atoms_simple.py
Simple examples of how to create atoms in an atomspace.  These
demonstrate two different ways in which the API can be used; one
creates atoms according to their types, the other creates them
directly.

## get_outgoings.py
Simple example to show how to access the outgoing set of a link.

## stop_go.py
A simple example of a "behavior tree" implemented in the AtomSpace.

## Scheme through python.
The following examples show how to use the scheme bindings through python

### scheme/atom_type_names.py
Example of how to obtain atom type names and atom type IDs.

### scheme/bindlink.py
Example of how to use the pattern matcher BindLink functionality.

### scheme/scheme_timer.py
Simple measurement of the performance of invoking the scheme (guile)
interpreter.
