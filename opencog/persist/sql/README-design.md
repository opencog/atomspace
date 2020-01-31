SQL Persist Design
==================
This reviews the design goals and philosophy behind the driver, as
well as a TODO list of open work items.

Design Goals
============
The goal of this implementation is to:

1) Provide OpenCog with a working memory, so that the AtomSpace could
   be stopped and restarted without requiring a data dump.  That is,
   checkpointing should be possible: if the AtomSpace crashes, data is
   not lost.  By using a database, a file format does not need to be
   invented. By using a database, data integrity is assured.
   By using incremental access, only those atoms that are needed get
   loaded into the AtomSpace; one does NOT have to do a bulk restore
   if one doesn't want to.

2) Provide an API for inter-server communications and atom exchange.
   Multiple AtomSpace can share data simply by sending atoms to,
   and retrieving atoms from the database.  Although this may not be
   the fastest way to send just single atoms, most algorithms do not
   need to send just single atoms: they just need to share some atoms,
   but its not clear which ones need to be shared.  Since all atoms are
   in the database, only the ones that are needed can be retrieved.

3) Provide a baseline/reference implementation by which other
   persistence designs can be measured. It is hoped that other systems
   would be at least as fast and as scalable as this one is: this is
   meant to provide a minimal function and performance level. The
   strength of the current design is supposed to be simplicity, not
   scalability or raw performance.

4) Provide a reference implementation for save and restore semantics.
   When saving or fetching outgoing sets, there are several choices
   of how to handle the associated values: these can also be saved
   or fetched, clobbering the AtomSpace contents, or some more
   fine-grained control can be provided.  The choices have both
   usability and performance implications. The choices are discussed
   in a separate section, below.

5) Discover the most minimal, simplest backing-store API. This API is
   the API between the AtomSpace, and the persistence backend.  The
   reason for keeping it as simple as possible is to minimize the work
   needed to create other backends, as well as all the standard software
   development reasons: lower complexity means fewer bugs and better
   performance.  Lower complexity means the code is easier to understand
   and use correctly.

6) A non-design-goal (at this time) is to build a system that can scale
   to more than 100 AtomSpace instances.  The current design might be
   able to scale to this many, but probably not much more.  Scaling
   larger than this would probably require a fundamental redesign of
   all of OpenCog, starting with the AtomSpace.

7) A non-design-goal is fully automatic save-restore of atoms.  The
   save and restore of atoms are performed under the explicit control
   by user-written code, invoking the save/restore API. There is no
   automation. Control is in the user's hands.

   The reason for this design point is that the AtomSpace is at too low
   a level to be fully automatic.  It cannot guess what the user really
   wants to do.  If it did try to guess, it would probably guess wrong:
   saving atoms before the user is done with them, saving atoms that get
   deleted microseconds later, fetching atoms that the user is completely
   disinterested in, clogging up RAM and wasting CPU time.  The policy
   for what to save, and when, needs to be left in control of the user
   algorithms.

   This layer only provides a mechanism.  It would be very very wrong to
   implement an automatic policy at this layer.

8) A fundamental non-goal is to provide any sort of generic object
   persistence.  The code here is meant only to save and restore atoms
   and values, and not generic C++ objects. The design of the AtomSpace
   has been carefully crafted to provide two classes of objects: the
   immutable, globally unique atoms, and the mutable valuations
   associated to atoms.  This backend mirrors this functional split.


Current Design
==============
The core design defines only a few very simple SQL tables, and some
reader and writer threads to save and restore atoms from an SQL database.

The current design can save/restore individual atoms, and it can
bulk-save/bulk-restore the entire contents of the AtomSpace. The above
listed goals seem to be met, more or less.

Features
--------
 * The AtomStorage class uses multi-threaded writeback queues for
faster async performance (see below for more info).

 * Fully automated mapping of in-RAM atoms to in-storage universal
unique identifiers (UUID's), using the TLB mechanism.

 * Multi-user issuance and management of UUID's is weakly tested and
seems to work. This allows a form of a "distributed atomspace" --
multiple AtomSpaces can connect to the same database at the same time,
and save and restore atoms, getting back the correct Values (such as
TruthValues) on each atom, as expected.

 * This implementation automatically handles clashing atom types.  That
is, if the data is written with one set of atom types, and then the
atomspace process is stopped, the atom types are all changed (with some
added, some deleted), then during the load of the old data, the types
will be automatically translated to use the new atom types. (The deleted
atom types will not be removed from the database.  Restoring atoms with
deleted atom types will cause an exception to be thrown.)

 * Non-blocking atom store requests are implemented.  Eight asynchronous
write-back threads are used, with hi/lo watermarks for queue management.
That is, if a user asks that an atom be stored, then the atom will be
queued for storage, and one of these threads will perform the actual
store at a later time. Meanwhile, the store request returns to the user
immediately, so that the user can continue without blocking.  The only
time that an atom store request blocks is if the queue is completely
full; in that case, the user will be blocked until the queue drains
below the low watermark. The number of connections can be raised by
editing the AtomStorage constructor, and recompiling.

This fire-n-forget queue management algo is implemented in the C++
`AtomStorage::storeAtom()` method.  If the backlog of unwritten atoms
gets too large, the storeAtom() method may stall. Its currently designed
to stall if there's a backlog of 100 or more unwritten atoms.  This can
be changed by searching for `HIGH_WATER_MARK`, changing it and recompiling.

Queue performance statistics can be printed with the `(sql-stats)`
scheme command.

 * Reading always blocks: if the user asks for an atom, the call will
not return to the user until the atom is available.  At this time,
pre-fetch has not been implemented.  But that's because pre-fetch is
easy: the user can do it in their own thread :-)


Semantics
=========
Exactly what to save, when saving and restoring atoms, is not entirely
obvious.  The alternatives, and their implications, are discussed below.

* Saving a single node. Should all associated values be saved? Should
the user get to pick which values get saved?  Its possible that the user
only wants to save one particular value, only, so as not to clobber
other values already in the database.  The current default is to save
all associated values, when storing a single node.  This is only weakly
unit-tested; the tests are not thorough, and do not check all possible
permutations.

* Saving a single link. When a link is saved, the outgoing set of the
link must also be saved. Thus, the above considerations for node-values
also apply to the outgoing set of the link, and so on, recursively, for
the nested links.  The current default is to save all associated values,
but only on the link itself, and NOT on the entire outgoing set, when
storing a single link.  This allows granular control on the part of the
user. This is only weakly unit-tested.

* Restoring a single node or link. The above considerations for saving
run in the opposite direction, when restoring. Thus, for example, when
restoring a single node, should all associated values in the AtomSpace
be clobbered, or not?

Currently, when an atom is restored, all of the associated values are
pulled from the database, and placed in the AtomSpace, clobbering the
previous AtomSpace contents.  For links, only the values on the specified
link are fetched, and none of the others.  This is tested, but only
weakly and incompletely, in the unit tests.

* Restoring by atom type; restoring incoming sets.  Groups of atoms
can be fetched from the database: in the first case, all atoms of a
given type; in the second case, all atoms in the incoming set of a
given atom.  There are four possibilities here: (a) fetch only the
atoms, but not any of the associated values. (b) fetch the atoms
and the associated values, but not the values in the recursive outgoing
sets. (c) fetch the atoms and values, and all atoms and values,
recursively, in their outgoing set. (d) fetch the atoms, but update
the values only if they are atoms are new to the atomspace; i.e. do
not clobber existing values in the atomspace.

Currently, option (b) is implemented, and is weakly unit-tested.
It is plausible that some users may want options (a), (c) or (d).
Note that option (d) has several variations.

In the SQL backend, option (b) mostly minimizes the network and database
traffic.  For other kinds of backends, it might be more efficient to
implement option (c), and just get all the data in one big gulp.

* Restoring by pattern. This is not implemented, not done.  However,
one can imagine a situation where a pattern-matcher-like interface
is provided for the backend, so that only certain values, on certain
atoms, in certain locations in a given pattern, are fetched.

This is not done because the pattern matcher is really quite complex,
and it seems kind-of crazy to try to put this in the backend.  There
currently aren't any plausible scenarios, and plausible algorithms,
that would need this capability.


Unfinished thoughts: Use JSONB
==============================
The storage problem for the atomspace is essentially the problem of
storing a graph, for which the EAV (Entity-Attribute-Value) pattern
seems to be the best fit.  See
https://en.wikipedia.org/wiki/Entity%E2%80%93attribute%E2%80%93value_model

A workable design point, using postgres 9.4 or newer, is JSONB. See
http://coussej.github.io/2016/01/14/Replacing-EAV-with-JSONB-in-PostgreSQL/

The goal here is to deal with how to store Values (ProtoAtoms).

The representation of the atomspace would then look vaguely like this
(this is a rough sketch):

CREATE TABLE atomspace (
    uuid  SERIAL PRIMARY KEY,
    type  INT,    ;; the atom type.
    atom  JSONB
);

A node would be:
{
   id:   42
   type:  3   ; a ConceptNode
   atom: {
       name:    "this is a node name"
   }
}

A link would be:
{
   id:   43
   type: 4     ; an OrderedLink
   atom: {
       outgoing:  [1,2,3]
   }
}

ProtoAtom aka Values updates would look like this (e.g. for stv==SimpleTruthValue)

UPDATE atomspace
SET atom = jsonb_set(properties, '{"stv"}', '[0.2, 0.54]')
WHERE id = 42;


Critically important observations:
----------------------------------
Performance depends crucially on the use of the containment (@>)
operator in the WHERE clause. This causes the GIN index to be used,
and thus can run thousands (!) of times faster than an ordinary
EAV table structure.


TODO
====
 * See also to-do list way up top.

 * Implement incoming-set caching. When an atomspace is too large to
   fit into RAM, we must leave some of it on disk.  However, it can
   become important to make sure that the incoming set of an atom has
   been loaded into RAM i.e. into the atomspace. Currently, repeatedly
   loading the incoming set is very slow and wasteful, and so its
   important to know whether an incoming set has already been loaded
   or not.  Technically, this caching can be done in the atomspace, I
   guess... See https://github.com/opencog/atomspace/issues/1373

 * Implement the large-outgoing-set extension. One way of doing this
   is touched in in https://github.com/opencog/atomspace/issues/1763
   The idea is this: Create a new link-type `ConsLink` or `ExtendLink`.
   A link with more than 330 atoms in the outgoing set would be split
   into several parts: the original link, with less than 330 atoms,
   the last atom of which would be the `ExtendLink`, which holds the
   next 330, etc. until they are all specified. This can be done with
   ZERO changes to the SQL table format.  Its really pretty easy to
   implement: just look at the length during save and restore, and
   trigger disassemble/reassembly code when the length limits are hit.
   Note also: the new link-type should be used in the SQL backend only,
   and thus, it would not be a real link-type, but a pseudo-type, a
   marker used only in the SQL tables.

 * Consider an alternate implementation, using JSONB to do an EAV-like
   storage: For details, see
   http://coussej.github.io/2016/01/14/Replacing-EAV-with-JSONB-in-PostgreSQL/

 * Extend the typecodes table to recording the type-inheritance
   hierarchy, so that types can be fully saved and restored from the
   database. Not clear how to resolve conflicts, if they occur, between
   the type inheritance hierarchy defined in C++, and the hierarchy
   defined in the database.  So maybe this is a *bad idea*.

 * Extend the standard table to hold count truth-values. Since almost
   all atoms have count TV's on them, this can lead to a more compact
   database format, and also could improve load and store performance,
   by reducing access to the valuations table.

 * Create custom table, tailored for EvaluationLink triples.
   Since majority of nodes/links in the DB will be in the form of
   EvaluationLink triples, a significant performance gain can be gotten
   by creating a custom table, and shunting queries to that.  This would
   decrease the SQL table sizes significantly, and decrease server I/O
   by factors of 2x-3x.  Another table, designed just for simple pairs,
   might help a lot, too.  This is easier said than done, however.

 * Add support for multiple atomspaces.

 * Create an API to allow the user to save only selected values on an
   atom.

 * Create an API to allow the user to NOT perform the recursive save of
   all values in the outgoing set (of a link).

 * Create an API to allow the user to restore only selected values on an
   atom.

 * Create an API to allow the user to NOT perform the recursive restore
   of all values in the outgoing set (of a link).

 * Create an API that provides fine-grained control over what values
   are fetched, when fetching atoms by type, or by incoming set.  See
   the section entitles "Semantics", above, for the various different
   options.

*** The End ***
