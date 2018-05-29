
Demo of Atoms, Values and the AtomSpace
=======================================
This directory contains examples that illustrate all of the main ideas
underlying the AtomSpace. This includes:

* Atoms, how to create them and how to use them.
* TruthValues, and how to associate them to atoms.
* A simple pattern-matching example.
* Techniques to control state in the AtomSpace.
* How to use Values to attach arbitrary data to specific Atoms.
* How to use time-varying (streaming) Values (experimental).

All of the examples are written in scheme.

Example Files
=============
It is recommended that you go through the examples in the order given.

* `basic.scm`   -- A very simple introduction
* `bindlink-example.scm` -- An example of using the pattern matcher.
* `python.scm`  -- loading python code from scheme.
* `execute.scm` -- An example of executing executable atoms, with
                   callbacks written in python or scheme.
* `get-put.scm` -- An example of asserting facts in the AtomSpace.
* `state.scm`   -- Maintaining unique state.
* `property.scm`-- Setting properies on Atoms.
* `values.scm`  -- Using Values and attaching them to Atoms.
* `filter.scm`  -- Filtering sets of atoms.
* `map.scm`     -- Applying a map function to a set or list.
* `recursive-loop.scm` -- An example of a tail-recursive loop.
* `random-choice.scm`  -- Numerical programming, including loops.
* `logger-example.scm` -- Using the built-in logger.
* `except.scm`  -- An example of exceptions being thrown and passed.
* `persist-example.scm` -- An example of saving atomspace data in an SQL
                   database.
* `stream.scm`  -- Using a stream of time-varying Values.
* `gperf.scm`   -- Some very crude performance measurements.


Introduction to Scheme and Guile
================================
Some minor familiarity with scheme is useful, but not required.
This section provides some hints and tips for getting the command
line working.

The variant of scheme used in OpenCog is that provided by guile.
Guile was chosen primarily because of its strong C/C++ inerfacing
capabilities.  Guile runs as an interpreter/compiler, providing a
read-evaluate-print loop (REPL), called `guile`. It is started at
the terminal shell prompt.

Before starting guile, you have to tell guile where to find the opencog
modules.  The best way to do this is to add the below to your `~/.guile`
file.  These will then run every time you start guile.
```
(add-to-load-path "/usr/local/share/opencog/scm")
(add-to-load-path ".")
```

To get your keyboard arrow keys to work, so that you can do command-line
editing, you should add the below to your `~/.guile` file as well.
```
(use-modules (ice-9 readline))
(activate-readline)
```

Finally, start guile:
```
$ guile
```
Next, load the opencog module:
```
(use-modules (opencog))
```
In the long run, it might be convenient to add the above to `~/.guile`
as well.

After the opencog module is loaded, you can create atoms "as usual" e.g.
```
(ConceptNode "asdf")
```

You can load other scm files (for example, "foobar.scm") by saying:

```
(load-from-path "foobar.scm")
```

Most functions have documentation, which can be viewed by saying
`,describe` and the name of the function.  Note the comma, and no
parentheses.  For example:
```
,describe cog-chase-link
```
Additional help can be gotten by saying
```
,apropos cog
```
and more generally
```
,help
```

See also opencog/guile/README or http://wiki.opencog.org/w/Scheme
for additional documentation.


List of the various OpenCog modules
===================================
A reasonably up-to-datelist of modules provided by OpenCog:
```
(use-modules (opencog))
(use-modules (opencog exec))
(use-modules (opencog logger))
(use-modules (opencog persist))
(use-modules (opencog persist-sql))
(use-modules (opencog query))
(use-modules (opencog rule-engine))
(use-modules (opencog ghost))
(use-modules (opencog atom-types))
(use-modules (opencog cogserver))
(use-modules (opencog nlp aiml))
(use-modules (opencog nlp chatbot))
(use-modules (opencog nlp chatbot-eva))
(use-modules (opencog nlp fuzzy))
(use-modules (opencog nlp lg-dict))
(use-modules (opencog nlp relex2logic))
(use-modules (opencog nlp sureal))
```
