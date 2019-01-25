
Demo of Atoms, Values and the AtomSpace
=======================================
This directory contains examples that illustrate all of the main ideas
underlying the AtomSpace. This includes:

* The AtomSpace as a knowledgebase - a graph database.
* Basic querying (pattern-matching) and inference on knowledge.
* Assigning degrees of truth various facts and inferences.
* Complex knowledge-representation tasks, including:
   -- Setting unique state
   -- Designing properties
   -- Assigning values and using key-value pairs efficiently
   -- Handling rapidly time-varying data (video/audio streams).

After this come examples for assorted advanced features that are
typically encountered:

* Creating a read-only base AtomSpace, with a read-write overlay.
* Throwing exceptions.
* Storing the AtomSpace in PostgreSQL.
* Distributed (network/cloud) AtomSpace.
* Using the logger.
* Calling Python from within the AtomSpace.
* Multi-threading with ParallelLink and JoinLink
* ... and much much more.

Most of the querying and pattern matching examples are in the
[**pattern-matcher**](../pattern-matcher) folder. Once you've gotten
a good idea of the basics from the demos here, go an explore the
examples there.

Introductory Examples
---------------------
It is recommended that you go through the examples in the order given.
These are "basic" demos that all users should know.

* `basic.scm`          -- How to start the guile shell.
* `knowledge.scm`      -- Representing knowledge.
* `bindlink.scm`       -- Queries and inference with the pattern matcher.
* `get-put.scm`        -- The two halves of a query: Get and Put.
* `assert-retract.scm` -- Asserting facts in the AtomSpace.
* `state.scm`          -- Maintaining unique state.
* `property.scm`       -- Designing Atoms with properties.
* `truthvalues.scm`    -- Declaring the truth of a proposition.
* `values.scm`         -- Using Values and attaching them to Atoms.
* `stream.scm`         -- Using a stream of time-varying Values.

There is an important collection of demos in the
[pattern-matcher](../pattern-matcher) folder. The pattern matching
demos are vital for understanding how to to be effective in writing
queries and formulating rules.

Advanced Demos
--------------
System programmers will need to know the following examples in order to
be effective.

* `recursive-loop.scm` -- Writing tail-recursive loops.
* `random-choice.scm`  -- Numerical programming, including loops.
* `logging.scm`        -- Using the cogutils logger.
* `python.scm`         -- Mixing Python and Scheme together.
* `execute.scm`        -- Callbacks written in python or scheme.
* `parallel.scm`       -- Multi-threading in Atomese.
* `except.scm`         -- Throwing and catching exceptions.
* `persistence.scm`    -- Layering the AtomSpace on a "real" database.
* `distributed.scm`    -- Distributed AtomSpace on multiple network nodes.
* `copy-on-write.scm`  -- Read-only AtomSpace, with r/w overlays.
* `gperf.scm`          -- Some very crude performance measurements.


Introduction to Scheme and Guile
--------------------------------
Some minor familiarity with scheme is useful, but not required.
This section provides some hints and tips for getting the command
line working.

The variant of scheme used in OpenCog is that provided by
[guile](https://www.gnu.org/software/guile/).
Guile was chosen primarily because of its strong C/C++ interfacing
capabilities.  Guile runs as an interpreter/compiler, providing a
read-evaluate-print loop (REPL), called `guile`. It is started at
the terminal shell prompt.

Before starting guile, you have to tell guile where to find the OpenCog
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

See also AtomSpace [guile wrapper README](../../opencog/guile/README)
or [OpenCog Scheme wiki page](https://wiki.opencog.org/w/Scheme) for
additional documentation.


List of the various modules
---------------------------
Here's a list of modules provided by the AtomSpace, listed in
alphabetical order. In general, you will use `(opencog query)`
and `(opencog exec)` the most frequently: these contain core
function that almost everything else depends on.
```
(use-modules (opencog))
(use-modules (opencog atom-types))
(use-modules (opencog attention-bank))
(use-modules (opencog exec))
(use-modules (opencog logger))
(use-modules (opencog persist))
(use-modules (opencog persist-sql))
(use-modules (opencog query))
(use-modules (opencog rule-engine))
```

There are other modules provided in other projects and repos. Here is
a reasonably up-to-date list of modules provided by OpenCog:
```
(use-modules (opencog agi-bio))
(use-modules (opencog cogserver))
(use-modules (opencog ghost))
(use-modules (opencog nlp aiml))
(use-modules (opencog nlp chatbot))
(use-modules (opencog nlp chatbot-eva))
(use-modules (opencog nlp fuzzy))
(use-modules (opencog nlp lg-dict))
(use-modules (opencog nlp relex2logic))
(use-modules (opencog nlp sureal))
```
