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
Finally, start guile:
```
$ guile
```

You may want to copy the below to your `~/.guile` file as well, or you
can copy them manually to the guile interpreter prompt:
```
(use-modules (ice-9 readline))
(activate-readline)
(use-modules (opencog))
```

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
(use-modules (opencog query))
(use-modules (opencog rule-engine))
(use-modules (opencog nlp lg-dict))
(use-modules (opencog nlp sureal))
(use-modules (opencog persist))
(use-modules (opencog persist-sql))
```

# Example Files

* basic.scm   -- a very simple introduction
* cog-bind-example.scm -- an example of using the pattern matcher.
* execute.scm -- an example of executing executable atoms, with
                 callbacks written in python or scheme.
* assign.scm  -- an example of asserting facts in the AtomSpace
* except.scm  -- an example of exceptions being thrown and passed
* persist-example.scm -- and example of saving atomspace data in an SQL
                 database.
* gperf.scm   -- some very crude performance measurements.
