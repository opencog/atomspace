
Demo of Atoms, Values and the AtomSpace
=======================================
This directory contains examples that illustrate all of the main ideas
underlying the AtomSpace. This includes:

* The AtomSpace as a knowledgebase - a graph database.
* Basic querying (pattern-matching) and inference on knowledge.
* Assigning degrees of truth to various facts and inferences.
* Complex knowledge-representation tasks, including:
    - Setting unique state
    - Designing properties
    - Assigning values and using key-value pairs efficiently
    - Handling rapidly time-varying data (video/audio streams).

After this come examples for assorted advanced features that are
typically encountered:

* Creating a read-only base AtomSpace, with a read-write overlay.
* Throwing exceptions.
* Storing the AtomSpace in RocksDB, PostgreSQL or plain-text files.
* Distributed (network/cloud) AtomSpace.
* Using the logger.
* Calling Python from within the AtomSpace.
* Multi-threading with ParallelLink and JoinLink
* ... and much much more.

Most of the querying and pattern matching examples are in the
[**pattern-matcher**](../pattern-matcher) folder. Once you've gotten
a good idea of the basics from the demos here, go and explore the
examples there. Return to the advanced examples here after exploring
the pattern engine.

Introductory Examples
---------------------
It is recommended that you go through the examples in the order given.
These are "basic" demos that all users should know. Open each of these
in your favorite text editor, and start reading. Cut-and-paste from the
text-editor to the guile prompt: this gives you a chance to see what
happens, and how the system reacts. (But read the "Introduction" below,
first).

* [`basic.scm`](basic.scm)          -- How to start the guile shell.
* [`knowledge.scm`](knowledge.scm)      -- Representing knowledge.
* [`bindlink.scm`](bindlink.scm`)       -- Queries and inference with the pattern matcher.
* [`get-put.scm`](get-put.scm)        -- The two halves of a query: Get and Put.
* [`assert-retract.scm`](assert-retract.scm) -- Asserting facts in the AtomSpace.
* [`state.scm`](state.scm)          -- Maintaining unique state.
* [`property.scm`](property.scm)       -- Designing Atoms with properties.
* [`truthvalues.scm`](truthvalues.scm)    -- Declaring the truth of a proposition.
* [`values.scm`](values.scm)         -- Using Values and attaching them to Atoms.
* [`stream.scm`](stream.scm)         -- Using a stream of time-varying Values.
* [`formulas.scm`](formulas.scm)       -- Representing arithmetic and computing Values.
* [`flows.scm`](flows.scm)          -- Flowing Values around.
* [`flow-formulas.scm`](flow-formulas.scm)  -- Dynamically updating TruthValues.
* [`flow-futures.scm`](flow-futures.scm)   -- Dynamically updating FloatValues.
* [`table.scm`](table.scm)          -- Fetching Values from a CSV/TSV table.
* [`multi-space.scm`](multi-space.scm)    -- Using multiple AtomSpaces at once.

After going through the above, go to the demos in the
[pattern-matcher](../pattern-matcher) folder. The pattern matching
demos are vital for understanding how to to be effective in writing
queries and formulating rules.  Then return to the advanced demos below.

Advanced Demos
--------------
System programmers will need to know the following examples in order to
be effective.

* [`recursive-loop.scm`](recursive-loop.scm) -- Writing tail-recursive loops.
* [`random-choice.scm`](random-choice.scm)  -- Numerical programming, including loops.
* [`factorial.scm`](factorial.scm)      -- Recursive numerical programming.
* [`logging.scm`](logging.scm)        -- Using the cogutils logger.
* [`python.scm`](python.scm)         -- Mixing Python and Scheme together.
* [`execute.scm`](execute.scm)        -- Callbacks written in python or scheme.
* [`threaded.scm`](threaded.scm)       -- Multi-threading in Atomese.
* [`parallel.scm`](parallel.scm)       -- Multi-threading in Atomese.
* [`except.scm`](except.scm)         -- Throwing and catching exceptions.
* [`persist-file.scm`](persist-file.scm)   -- Dump and load Atoms to a plain-text file.
* [`persist-store.scm`](persist-store.scm)  -- StorageNode API to a plain-text file.
* [`persistence.scm`](persistence.scm)    -- StorageNode API to an SQL database.
* [`persist-query.scm`](persist-query.scm)  -- Fetching sets of Atoms with queries.
* [`persist-multi.scm`](persist-multi.scm)  -- Work with multiple databases/servers at once.
* [`persist-proxy.scm`](persist-proxy.scm)  -- Work with proxy agents to access multiple dbs.
* [`copy-on-write.scm`](copy-on-write.scm)  -- Read-only AtomSpace, with r/w overlays.
* [`frame.scm`](frame.scm)          -- Using StateLink in overlays.
* [`gperf.scm`](gperf.scm)          -- Some very crude performance measurements.

Documentation
-------------
The wiki provides more detailed documentation for each of the AtomSpace
Atom types, and more. Some good places to start are here:

* [AtomSpace](https://wiki.opencog.org/w/AtomSpace)
* [Atom types](https://wiki.opencog.org/w/Atom_types)
* [Pattern matching](https://wiki.opencog.org/w/Pattern_matching)


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

It is convenient to preconfigure guile to make things smoother. One is
to enable automated command-line editing (i.e. get the arrow keys to
work). Do this by editing `~/.guile` and adding the lines:
```
(use-modules (ice-9 readline))
(activate-readline)
```
The above will be auto-run every time you start guile.

Finally, start guile:
```
$ guile
```
Next, load the opencog module:
```
(use-modules (opencog))
```
If the above does not work, please see the next section.

After the opencog module is loaded, you can create atoms "as usual" e.g.
```
(ConceptNode "asdf")
```

You can load other scm files (for example, "foobar.scm") by saying:

```
(load "./foobar.scm")
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

Common Faults
-------------
If you get the error:
```
> (use-modules (opencog))
While compiling expression:
no code for module (opencog)
```
Make sure you did not forget to say `sudo make install`. If that does
not fix it, make sure that either `/usr/share/guile/site/3.0/opencog.scm`
or `/usr/local/share/guile/site/3.0/opencog.scm` exist.  It might also
have been installed yet somewhere else; find it.  Next, verify that
guile is looking in the right places:
```
> %load-path
```
should print
```
("/usr/share/guile/3.0" "/usr/share/guile/site/3.0" "/usr/share/guile/site" "/usr/share/guile")
```
or something similar.  If the correct path is not listed, then add it:
```
(add-to-load-path "/usr/local/share/guile/site/3.0")
```
or wherever it is to be found. That should be enough to fix things.
To make this permanent, add this to the `~/.guile` file.

List of the various modules
---------------------------
Here's a list of modules provided by the AtomSpace, listed in
alphabetical order. In general, you will use `(opencog exec)`
the most frequently: this contains core functions that almost
everything else depends on.
```
(use-modules (opencog))
(use-modules (opencog atom-types))
(use-modules (opencog csv-table))
(use-modules (opencog exec))
(use-modules (opencog logger))
(use-modules (opencog matrix))
(use-modules (opencog persist))
(use-modules (opencog persist-cog))
(use-modules (opencog persist-file))
(use-modules (opencog persist-rocks))
(use-modules (opencog persist-sql))
(use-modules (opencog python))
(use-modules (opencog randgen))
(use-modules (opencog sheaf))
(use-modules (opencog test-runner))
(use-modules (opencog type-utils))
(use-modules (opencog uuid))
```

There are other modules provided in other projects and repos. Here is
a list of some of the key, important (supported and active) modules
in other git repos:
```
(use-modules (opencog cogserver))
(use-modules (opencog learn))
(use-modules (opencog nlp))
(use-modules (opencog nlp lg-dict))
(use-modules (opencog nlp lg-export))
```

A list of important modules that are taking a break, waiting to spring
into action for when they are next needed:
```
(use-modules (opencog agi-bio))
(use-modules (opencog asmoses))
(use-modules (opencog bioscience))
(use-modules (opencog cheminformatics))
(use-modules (opencog generate))
(use-modules (opencog pln))
(use-modules (opencog ure))
(use-modules (opencog vision))
```

These modules are more or less obsolete and are no longer being
maintained. It seems unlikely that they will ever be maintained
again.
```
(use-modules (opencog attention))
(use-modules (opencog attention-bank))
(use-modules (opencog ghost))
(use-modules (opencog miner))
(use-modules (opencog nlp aiml))
(use-modules (opencog nlp chatbot))
(use-modules (opencog nlp chatbot-eva))
(use-modules (opencog nlp fuzzy))
(use-modules (opencog nlp microplanning))
(use-modules (opencog nlp relex2logic))
(use-modules (opencog nlp sureal))
(use-modules (opencog octomap))
(use-modules (opencog openpsi))
(use-modules (opencog reduct))
(use-modules (opencog spacetime))
```
