Guile usage examples
--------------------

See also opencog/guile/README or http://wiki.opencog.org/w/Scheme
for additional documentation.

Before starting guile, you have to tell guile where to find the opencog
modules.  The best way to do this is to add the below to your `~/.guile`
file.  These will then run every time you start guile.
```
(add-to-load-path "/usr/local/share/opencog/scm")
(add-to-load-path ".")
```

If you want your arrow keys to work, so that you can do command-line
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

You can load other scm files (for example, "utilities.scm") by saying:

```
(load-from-path "utilities.scm")
```

Some, but not all functions have documentation, which can be viewed by
saying `,describe` and the name of the function.  Note the comma, and no
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

#List of modules
The current list of modules that wrap C++ code includes:
```
(use-modules (opencog))
(use-modules (opencog logger))
(use-modules (opencog query))
(use-modules (opencog exec))
(use-modules (opencog rule-engine))
(use-modules (opencog nlp lg-dict))
(use-modules (opencog nlp sureal))
(use-modules (opencog persist))
(use-modules (opencog persist-sql))
```

# Example Files

* `basic.scm`   -- A very simple introduction
* `bindlink-example.scm` -- An example of using the pattern matcher.
* `execute.scm` -- An example of executing executable atoms, with
                   callbacks written in python or scheme.
* `get-put.scm` -- An example of asserting facts in the AtomSpace.
* `state.scm`   -- Maintaining unique state.
* `property.scm`-- Setting properies on atoms.
* `filter.scm`  -- Filtering sets of atoms.
* `map.scm`     -- Applying a map function to a set or list.
* `random-choice.scm`  -- Numerical programming, including loops.
* `logger-example.scm` -- Using the built-in logger.
* `except.scm`  -- An example of exceptions being thrown and passed.
* `persist-example.scm` -- An example of saving atomspace data in an SQL
                   database.
* `gperf.scm`   -- Some very crude performance measurements.
