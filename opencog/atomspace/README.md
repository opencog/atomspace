
AtomSpace Design Notes
======================
This directory contains the code for the AtomSpace "itself", minus
everything else. The AtomSpace "itself" is just a class for tracking
all the atoms in the system, and making sure that they are unique.
There's actually not much to it. It's a replacable component, and
could be replaced with something else, something fancier, as long
as you keep the API.

However, this README also discusses the design tradeoffs that were made
that got us to here. All of these design tradeoffs are inter-related
and tangled. There's a reason things got to be the way they are.

-------------------------
Atom Implementation Notes
-------------------------
The `Link` class explicitly contains the outgoing set of the Link.
The `Atom` class explicitly contains the incoming set of the Atom.
Smart pointers (the C++ `std::shared_ptr` class) are used for holding
Atoms.  The `Handle` class is a wrapper around `std::shared_ptr`.
The Link contains an `std::vector` of Handles.  Thus, to avoid memory
management issues, the Atom uses weak pointers for the incoming set.

------------------------------
AtomSpace Implementation Notes
------------------------------
The uniqueness constraint on Atoms implies that the AtomTable::addAtom()
method is fairly complicated: it must be able to detect if an atom
being added already exists in the atomspace.  The addAtom() method
implements the "merge procedure", page 68 of Baader&Nipkow; the
soundness of the proceedure is given in theorem 4.4.3.  A lot of the
complexity of atoms and the atomspace centers around trying to make
the merge algorithm efficient.  It is the primary bottleneck of the
current implementation.

The AtomSpace is crudely "distributed", i.e. parts of it can reside on
a networked collection of computers.  It is "crude" because the current
architecture works, but is not very fast or efficient.  The AtomTable
holds the local, in-RAM cache of atoms on this computer. The AtomSpace
is a wrapper around the AtomTable, and provides a backing store
(persistance) for the atomtable.  Depending on which backend is used,
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
A prototype using garbage collections (the Boehem GC) was attempted, but
this did not work well, due to the bad interplay between std:: container
classes and weak pointers.  Basically, every atom stores both it's
outgoing set (atoms it points to) and it's incoming set (atoms that
point at it). This means that there are circular loops everywhere. Now,
the Boehm GC can discover circular loops just fine, but only if they are
reasonably short. The problem is that std::set can have pointers to
pointers to pointers, of depth log(N), and N can get quite large,
larger than what bdgc can easily detect. In essence, std::set does not
play nice with bdgc. Bummer. So reference counting is done instead, and
in order to break the circular loops, the incoming set consists of weak
pointers. More about garbage collection below.

------------------------------
Valuation Implementation Notes
------------------------------
Valuations are currently stored in a ValuationSpace, but it might be
better to store them directly, in each atom.

TruthValues are stored directly with each Atom; this could be changed
to use the generic Valuation mechanism, but this would incurr
significant(?) performance penalty.

Valuations are currently immutable, this too has performance and access
penalties that perhaps should be re-thought?


-------------------------
Garbage Collection Design
-------------------------

As of October/November 2013, Handles use `std::shared_pointer` to deal
with memory management, and so using GC is no longer urgent.  The
shared_pointers seem to work OK, for now.  Based on performance
measurements, however, shared pointers are 3x slower than GC could be
(compare the AtomTable results for April and November 2013 in the
`opencog/benchmark/diary.txt` file, which show a rough 3x slowdown.)

Without garbage collection (or smart pointers) it becomes unsafe to use
bare pointers in a multi-threaded environment.  With garbage collection
(or smart pointers), and a reasonably good design and coding style, the
use of bare pointers in a multi-threaded environment becomes safe. The
problem with smart pointers is that they are (1) generally slower, and
(2) use significantly more RAM (40 bytes per pointer, instead of 8 bytes,
as well as additional bytes in the object, for the counter itself.)

A prototype using BDW-GC was attempted in 2014, but was found to have
problems.  The main problem is that the `std::` container classes are
just not freindly for garbage collection.  Consider, for example, the
`std::set` container. If it has N objects in it, it will have log_2(N)
pointers (assuming a binary-tree implementation).  If N = 1 million,
then tree depth will have a depth of about log_2(1M) = 20. If the
terminal object (e.g. an Atom) has a back-pointer to another atom
(e.g. the IncomingSet), this will result in a cycle that is 21 pointers
in circumfrence.  Now, BDW-GC can detect cycles, but, byt default, it
only looks for relatively short cycles (I think it goes up to 6 or 8
or 10 or something like that). Thus, it effectively is unable to detect
such large cycles, and thus, cannot free the resulting memory!  Perhaps
it would be possible to make the std::c++14 container classes be more
GC-freindly, to avoid these issues, but this is way beyond what the
OpenCog project can do.  In essence, current C++ is more or less
incompatible with, even hostile to, garbage collection.

Thus, we deal with the seemingly slower and klunkier reference-counting
design.  It works well-enough.  As long as we stick with C++ for the
core AtomSpace, I don't see a better way.  It is far from clear that
any other language has a better solution, anyway.


----------------
Threading Design
----------------

As of November 2013, all atomspace operations should be thread-safe.
This includes all AtomSpace API calls, and all public methods on Atoms,
Links, Nodes, truth and attention values.  Thread-safety is mildly
tested in AtomSpaceAsyncUTest but more robust threading tests would be
great.  In addition, comprehensive multi-threaded benchmarks are sorely
needed.

The AtomTable::getHandlesByXXX() methods offer a great opportunity for
adding parallelism.  Currently, they use std::copy_if(), which can be
replaced by OMP_ALGO versions, and including oc_omp.h. For examples on
how this is done in practice, grep the moses code. Its actually quite
very easy; I haven't done so out of laziness mostly (and the greedy
desire for a benchmark).

The AtomTable uses a single global lock, and it potentially causes
significant contention in a highly-threaded environment. It is not
clear how to fix this.  Converting it to a reader-writer lock is not
a solution, because reader-writer locks are much larger, while also
requiring that any cache-lines holding the locks be cleared,
synchronized.  Thus, reader-writer locks don't avoid any of the
cache-contention bottlenecks that ordinary plain-simple mutexes have,
while at the same time being fatter and klunkier.

Some speedup might be possible if each index used it's own private lock,
maybe. Maybe not.  Some speedup might be possible if index insertion was
done asynchronously (i.e. in service threads). Maybe. Unclear. That
entails extra complexity.  A multi-threaded benchmark is needed.

The atoms are all using a per-atom lock, and thus should have no
contention (although this is a bit RAM-greedy, but what the heck --
the alternative of one global lock would be a huge bottleneck.)

The ClassServer() uses a mutex when fetching info.  If would be great
to make this lockless somehow, since, realistically, as, currently,
absolutely no one ever adds new atom types, once the cogserver has been
initialized. i.e. we're using a lock to protect a case that never
happens in real life.

-----
