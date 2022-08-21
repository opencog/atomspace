
ProtoAtoms [now renamed Value]
==========
ProtoAtoms aka Values provide a common base class for both Atoms and Values.
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
The API for time-varying value streams is still experimental. What is
currently implemented in StreamValue and RandomStream is an experiment.
Known deficiencies:
* We need a way of streaming something other than vectors of floats.

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
