
Values
======
The class Value provides a common base class for both Atoms and Values.
The aim of having a common base class for both kinds of objects is to
simplify general handling in various different subsystems.

Atoms and Values can be contrasted in several ways; in brief, they
offer two very different modes/styles of storing information, with
very different performance profiles, API's and use-cases. Atoms
are bigger, slower, bulkier but a lot more powerful; the are best
used for representing graphs that need to be repeatedly traversed.
Values are fleeting and fast, but ill-suited for graphs. Values are
ideal for holding data streams (e.g. video, audio).

Hierarchy:
* Atoms are a special case of Values; wherever you can use an Value,
  you can use an Atom.

Atoms:
* Atoms are heavy-weight, slow to create, hard/impossible to destroy.
* Atoms are immutable; they cannot be changed after being created.
* Atoms are globally unique (guaranteed by the Atomspace).
* Atoms are indexed, and are thus searchable by name or type, and by
  the pattern-matching subsystem.
* Atoms are meant to represent graphs (that are frequently traversed).

Values:
* Values are light-weight, fast and easy to create/destroy.
* Values are not (globally or locally) unique.
* Values are not indexed, and can be found only by knowing their key.
* Values are meant to hold transient and streaming data (e.g. video)

Streams:
* Streams are time-varying values. Experimental.
* RandomValue for an example of a stream
  - every time its called, it returns a different value.

Databases:
* Atoms can be stored in the Atomspace; Values cannot.
* Values can be stored in Atoms (can be associated to Atoms).

Every Atom is essentially a distinct key-value (noSQL) database that
can store more-or-less arbitrary Values. This database has all the
typical performance properties and usability properties that one might
expect from a key-value store.

The AtomSpace is a generic graph database that holds Atoms. This is
because every graph can be decomposed into trees, and because Atoms
form a tree (with Nodes as leaf vertices, and Links as interior
vertices). There are also many other domain-specific ways of viewing
Atoms as forming a graph; in all cases, these graphs are held in the
AtomSpace.

Thus the AtomSpace can be viewed as a "database of databases". The
AtomSpace has a powerful query mechanism (the pattern matcher), whereas
Values can only be found by knowing which Atom they are in, and which
Key they are filed under. This two-level structure hopefully provides
the kind of richness and flexibility suitable for complex problems.

Streams
-------
The API for time-varying value streams is still experimental, but seems
to be working OK. Besides plan-old `StreamValue` and `RandomStream`,
assorted more complex Values, such as `FormulaStream`, `LinkStream`
and `FormulaTruthValue` seem to work well. They're even used for
computing the dot-products of two vectors, on the fly.

Streams do not currently have any kind of chunking, update or buffering
policy. A stream can provide samples from a constantly-changing stream,
or it can provide buffered I/O. Samples are appropriate for high
data-rate streams, where a sample of a recent value is desired.
Buffered I/O is appropriate for loss-less streams, which provide a
guarantee that everything is delivered, in the order in which it
occurred, and nothing is dropped. Buffered I/O can, of course,
bottleneck if the consumer is slower than the producer.

Sampling is appropriate for environmental sensors: e.g. is a door open
or closed, **right now**? There is no need to buffer a million samples
per second of some open/close sensor.

Buffering is appropriate for reading texts. One wants each word, in
order, and the book can wait, without overflowing, until the reader
is ready.

The `QueueValue` provides a FIFO that blocks the writer is the queue is
full, and blocks the reader if the queue is empty.

One can imagine a very rich architecture for streams. This is not being
provided in this, the core AtomSpace repo. So far, only the simplest
streaming primitives are provided, as seem appropriate for basic current
apps. Richer concepts of streams would include (atomic-update) mailboxes,
message-passing systems, etc. The
[OpenCog Sensory](https://github.com/opencog/sensory) project examines
some of these.

The biggest design issue is that the usage of the `update()` method is a
bit haphazard. It's OK for sampling streams, but abused for buffered
streams.  For example, `update()` is called by the `to_string()` method,
so if you call `to_string()` from some debugging code, buffered data
will be lost.  Clearly, more careful design is required.


Names
-----
The word "Atom" comes from the idea of an "atomic sentence", in formal
logic.  Atoms are more-or-less the same thing as the "terms" of "term
algebra".

The word "Value" comes from the concept of "valuation" in formal logic
and model theory. So, in model theory, a valuation is an assignment of
truth values to each and every term; valuations indicate which terms are
true, and which are false.

The design goal here is to allow multiple, different valuations at the
same time (indexed by the "valuation key"), while also generalizing
valuations from binary true/false values to Bayesian or frequentist
probabilities (floating point numbers) or any kind of more general value
(thus, a general-purpose key-value store).

Status
------
Almost everything has been implemented; a few minor work items remain.

The current implementation status is tracked in
[github bug #513](https://github.com/opencog/atomspace/issues/513)

Adding New Atom and Value Types
-------------------------------
Please see the
[README-Adding-New-Atom-Types.md](../atom_types/README-Adding-New-Atom-Types.md) file.

See also the [Custom Types Example](../../../examples/type-system/README.md)

TODO
----
* Perhaps add a TypeValue, which would be a vector of Types.  If could
  be useful as a kind-of table signature (for the csv table handling
  code).

History and Bibliography
------------------------
The first attempt at this was in the "WIRES" system, from December 2008.
It was in the `opencog/scm/wires` directory, which is being removed in
the same git commit as the one adding this remark. It can still be
[viewed here](https://github.com/opencog/atomspace/tree/3f58a2cdd7891da074ee48bd517c7f656ff12b14/opencog/scm/wires).
That work was originally conceived as a constraint-type wiring system,
inspired by SICP sections 3.3 (constraints) and 3.5 (streams).

* SICP is "Structure and Interpretation of Computer Programs",
  Abelson & Sussman. MIT Press

Additional inspiration was drawn from:

* *The Art of the Propagator*
  Alexey Radul; Gerald Jay Sussman
  local: MIT-CSAIL-TR-2009-002
  http://dspace.mit.edu/handle/1721.1/44215

Over time, these ideas evolved into two distinct subsystems: the query
system, and dynamically changing Values. The query system, AKA the pattern
matcher, solved the issue of finding complex "wiring diagrams" in the
AtomSpace, and determining possible groundings, and queuing them up
into a QueueValue result.

Separately, the idea of TV's that "flow" through the hypergraphs morphed
into the concept of dynamically-changing, real-time Values. The
implementation for this was spurred by Alexey Potopov's demand to run
some deep-learning neural-net image-processing data through the AtomSpace.
