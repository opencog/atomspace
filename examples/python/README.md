Python Examples
===============

To run these examples, you must first set the `PYTHONPATH` to the
atomspace python install directory.  In most cases, the following will
suffice:

```
export PYTHONPATH=$PYTHONPATH:/usr/local/share/opencog/python
```

The opencog atomspace cython modules are installed here:
```
/usr/local/share/opencog/python/opencog
```

From the python prompt, the following should list the python
opencog modules:
```
help('opencog')
```
The contents of a single module can be viewed by saying, for example:
```
import opencog.atomspace
print dir(opencog.atomspace)
```


## create_atoms_by_type, create_atoms_simple

Simple examples of how to create atoms in an atomspace.
These demonstrate two different ways in which the API can be used;
one creates atoms according to thier types, the other creates them
directly.

## bindlink

Example of how to use the pattern matcher BindLink functionality.

## atom_type_names

Example of how to obtain atom type names and atom type IDs.

## stop_go

A simple example of a "behavior tree" implemented in the AtomSpace.

## scheme_timer

Simple measurement of the performance of invoking the scheme
(guile) interpreter.
