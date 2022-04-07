
AtomSpace Design Notes
======================
This directory contains the code for the AtomSpace "itself", minus
everything else. The AtomSpace "itself" is just a class for tracking
all the atoms in the system, and making sure that they are unique.
It manages two basic indexes, allowing atoms to be found quickly,
by name and by type.  There's actually not much to it. It's a
replaceable component, and could be replaced with something else,
something fancier, as long as you keep the API.

This README also discusses the design tradeoffs that were made that got
us to here. All of these design tradeoffs are inter-related and tangled.
There's a reason things got to be the way they are.

-------------------------
Atom Implementation Notes
-------------------------
The `Link` class explicitly contains the outgoing set of the Link.
The `Atom` class explicitly contains the incoming set of the Atom.
Smart pointers (the C++ `std::shared_ptr` class) are used for holding
Atoms.  The `Handle` class is a wrapper around `std::shared_ptr`.
The Link contains an `std::vector` of Handles.  Thus, to avoid memory
management issues, the Atom uses weak pointers for the incoming set.

The above seems like the simplest, easiest, most compact and fastest
way to implement Atoms.  Its not carved in stone, but it seems to work
well.

The benchmark code in the
[opencog/benchmark](https://github.com/opencog/benchmark) repo is
handy for verifying if your alternative design/implementation makes
things better or worse.

------------------------------
Valuation Implementation Notes
------------------------------
Valuations are stored in a C++ `std::map` container -- basically, a
key-value database -- in each atom.

TruthValues and AttentionValues are layered on top of FloatValue; a
FloatValue is just a sequence of doubles (`std::vector<double>`).

Valuations are immutable. It seemed like it was just easier to make
them immutable, rather than adding a mutex to each and every one.
Adding a mutex would bloat each valuation with more RAM usage.  Also,
mutexes are complex, prone to deadlocks, and can surprise users with
unexpected data-ordering access issues.

Instead, serialization is maintained by allowing the user to change
the entries in the `std::map`, and guarding that with a mutex. Thus,
the key-value store can be freely changed at any time, in a thread-safe
fashion that is easy to audit and verify for correct behavior.

Although the above says "valuations are immutable", the streaming
valuations can be rapidly time-varying. The can hold video and audio
streams, for example, or other streams, such as 3D positional data
in the environment.

The above seems like the simplest, easiest, most compact and fastest
way to implement Values.  Its not carved in stone, but it seems to work
well.


---------------------------------
AtomSpace Implementation Overview
---------------------------------
The uniqueness constraint on Atoms implies that the AtomTable::addAtom()
method is fairly complicated: it must be able to detect if an atom
being added already exists in the atomspace.  The addAtom() method
implements the "merge procedure", page 68 of Baader&Nipkow; the
soundness of the procedure is given in theorem 4.4.3.  A lot of the
complexity of atoms and the atomspace centers around trying to make
the merge algorithm efficient.  It is the primary bottleneck of the
current implementation.

The AtomSpace is crudely "distributed", i.e. parts of it can reside on
a networked collection of computers.  It is "crude" because the current
architecture works, but is not very fast or efficient.  The AtomTable
holds the local, in-RAM cache of atoms on this computer. The AtomSpace
is a wrapper around the AtomTable, and provides a backing store
(persistence) for the AtomTable.  Depending on which backend is used,
atomtables on different machines can share a common database.

Multiple AtomSpaces can be used simultaneously, and AtomSpaces can be
created in a hierarchical fashion.  Thus, the AtomSpace can be thought
of as an "environment" (kind-of like the environment in bash, but
hierarchical, so, more like the environment in Lisp or Scheme.)  The
hierarchical nature means that Links in the child atomspace can refer
to Atoms in the parent atomspace (but not vice-versa).

The AtomSpace API is completely thread-safe. This includes the methods
on class Atom and class TruthValue: these can be accessed by any number
of threads simultaneously. Several unit tests test for this. More about
threading below.

Memory management for Atoms is handled by reference-counting, using the
std::shared_ptr class. This means that you should never use the new()
and delete() operators directly on Atoms or TruthValues. An Atom will
automatically be destroyed when it's reference counter drops to zero.
A prototype using garbage collections (the Boehm GC) was attempted, but
this did not work well, due to the bad interplay between std:: container
classes and weak pointers.  Basically, every atom stores both it's
outgoing set (atoms it points to) and it's incoming set (atoms that
point at it). This means that there are circular loops everywhere. Now,
the Boehm GC can discover circular loops just fine, but only if they are
reasonably short. The problem is that std::set can have pointers to
pointers to pointers, of depth log(N), and N can get quite large,
larger than what BDWGC can easily detect. In essence, `std::set` does not
play nice with BDWGC. Bummer. So reference counting is done instead, and
in order to break the circular loops, the incoming set consists of weak
pointers. More about garbage collection below.

-------------------------
Multiple AtomSpace Design
-------------------------

**See also the [DeepSpace README](README-DeepSpace.md) for more.**

Overlay AtomSpaces are needed in (at least) these situations:

* Pattern Matching -- Need to hold temporary results computed during
  the traverse, which may be discarded later, if the traverse is
  unsuccessful (unsatisfiable).  Temporary atoms arise in several
  different ways, including by black-box user-written code that might
  get triggered during the traverse phase.  The pattern matcher creates
  a temporary "transient" atomspace, which is cleared at the end of
  each traverse.

* Large dataset management -- A particularly large dataset (perhaps too
  large to fit in RAM) is shared by multiple users. It is so large that
  users want to avoid making private copies. Yet, since its shared,
  updates must be disallowed.  Thus, each user gets a read-write
  overlay, with the underlying base-space marked read-only. Since the
  base-space is an in-RAM cache of what's on disk, it should still be
  possible to load the base-space with atoms from disk, and to remove
  atoms from the base-space, to free up RAM, all without violating its
  read-only nature.  Examples include: large genomic datasets; Sophia
  robot character personality files.

The above are requirements; the base and overlay atomspaces should
behave as intuitively described.  In practice, this means:

* When a user alters a Value (TruthValue, AttentionValue, or other) in
  the overlay, it should *not* clobber the Value in the base space.

How can this be implemented?

### Design A)
A copy-on-write is performed, so that if a read-only Atom in the
base-space is altered, a copy is created in the overlay. The copy
has it's own key-value store, which can be freely altered, without
disturbing the base.

This design makes graph traversal hard: the copied atom fails to appear
in the outgoing set of any Link that the base atom is in. Likewise,
the incoming set of the copied atom is empty. There are two different
partial solutions:

#### Design A1)
In addition to copying an atom, copy it's entire incoming set. This has
a potentially huge negative performance impact on RAM usage.

#### Design A2)
Alter the atom's `getIncomingSet()` method, so that it returns not only
it's own strict incoming set, but also that of any atoms that it's
hiding. The cost here seems to be minimal.

Add a "masked" bit-flag to the atom, indicating that there's another
atom in an overlay that is covering it. This can be used to avoid
traversing covered/masked atoms.

### Design B)
Each Atom holds an atomspace, key, value triple, so that, to find the
value, both the key, and the relevant atomspace must be supplied.

This has several downsides -- a run-time performance penalty for every
lookup (because a more complex lookup) and an API penalty: its not
enough to just have the atom in hand; one must also specify the
atomspace.

### Design C)
Store the key-value DB's in the atomspace (and not in the atom).
This has several downsides -- a space penalty (a hash table or b-tree in
the atomspace) and a time penalty (a hash/btree lookup).  This might be
OK, if most atoms had no values at all attached to them.  However, for
all existing applications, the majority of atoms have either TV's or
AV's or both.

------
Conclusion: Design A2 seems like the best, for now.

However, see also the [DeepSpace README](README-DeepSpace.md) for more
info.

-------------------------
Garbage Collection Design
-------------------------

As of October/November 2013, Handles use `std::shared_pointer` to deal
with memory management, and so using GC is no longer urgent.  The
shared pointers seem to work OK, for now.  Based on performance
measurements, however, shared pointers are 3x slower than GC could be
(compare the AtomTable results for April and November 2013 in the
[benchmark/diary.txt](https://github.com/opencog/benchmark/blob/master/atomspace/atomspace/diary.txt)
file, which show a rough 3x slowdown.)

Without garbage collection (or smart pointers) it becomes unsafe to use
bare pointers in a multi-threaded environment.  With garbage collection
(or smart pointers), and a reasonably good design and coding style, the
use of bare pointers in a multi-threaded environment becomes safe. The
problem with smart pointers is that they are (1) generally slower, and
(2) use significantly more RAM (40 bytes per pointer, instead of 8 bytes,
as well as additional bytes in the object, for the counter itself.)

A prototype using BDW-GC was attempted in 2014, but was found to have
problems.  The main problem is that the `std::` container classes are
just not friendly for garbage collection.  Consider, for example, the
`std::set` container. If it has N objects in it, it will have log_2(N)
pointers (assuming a binary-tree implementation).  If N = 1 million,
then tree depth will have a depth of about log_2(1M) = 20. If the
terminal object (e.g. an Atom) has a back-pointer to another atom
(e.g. the IncomingSet), this will result in a cycle that is 21 pointers
in circumference.  Now, BDW-GC can detect cycles, but, by default, it
only looks for relatively short cycles (I think it goes up to 6 or 8
or 10 or something like that). Thus, it effectively is unable to detect
such large cycles, and thus, cannot free the resulting memory!  Perhaps
it would be possible to make the std::c++14 container classes be more
GC-friendly, to avoid these issues, but this is way beyond what the
OpenCog project can do.  In essence, current C++ is more or less
incompatible with, even hostile to, garbage collection.

Thus, we deal with the seemingly slower and clunkier reference-counting
design.  It works well-enough.  As long as we stick with C++ for the
core AtomSpace, I don't see a better way.  It is far from clear that
any other language has a better solution, anyway.


----------------
Threading Design
----------------

As of November 2013, all atomspace operations are thread-safe.  This
includes all AtomSpace API calls, and all public methods on Atoms,
Links, Nodes, Truth and Attention values.  Thread-safety is mildly
tested in `AtomSpaceAsyncUTest` but more robust threading tests would be
great.  In addition, comprehensive multi-threaded benchmarks are sorely
needed.

The `AtomTable::getHandlesByXXX()` methods offer a great opportunity for
adding parallelism.  Currently, they use `std::copy_if()`, which can be
replaced by `OMP_ALGO` versions, and including `oc_omp.h`. For examples
showing how this is done in practice, grep the MOSES code. Its actually
quite very easy; I haven't done so out of laziness mostly (and the greedy
desire for a benchmark).

The AtomTable uses a single global lock, and it potentially causes
significant contention in a highly-threaded environment. It is not
clear how to fix this.  Converting it to a reader-writer lock is not
a solution, because reader-writer locks are much larger, while also
requiring that any cache-lines holding the locks be cleared,
synchronized.  Thus, reader-writer locks don't avoid any of the
cache-contention bottlenecks that ordinary plain-simple mutexes have,
while at the same time being fatter and clunkier.

Some speedup might be possible if each index used it's own private lock,
maybe. Maybe not.  Some speedup might be possible if index insertion was
done asynchronously (i.e. in service threads). Maybe. Unclear. That
entails extra complexity.  A multi-threaded benchmark is needed.

The atoms are all using a per-atom lock, and thus should have no
contention (although this is a bit RAM-greedy, but what the heck --
the alternative of one global lock would be a huge bottleneck.)

The NameServer() uses a mutex when fetching info.  If would be great
to make this lock-less somehow, since, realistically, as, currently,
absolutely no one ever adds new atom types, once the cogserver has been
initialized. i.e. we're using a lock to protect a case that never
happens in real life.

-----
