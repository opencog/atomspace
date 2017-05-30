
AtomSpace README
----------------

This README contains miscellaneous notes about the atomspace. But first:

What is an Atom?
================
An Atom is an immutable data structure that can be used to represent
knowledge. Atoms are designed to be so general that any kind of domain
knowledge can be represented with them, including classical predicate
logic, natural language, Bayesian probability networks, neural networks,
and so on. The basic representation is that of "typed hypergraphs". Each
atom has a "type" (in the sense of "type theory") -- there are currently
over one hundred pre-defined types, such as "and", "or", "not", but also
"word", "sentence", as well as "implication" and "inheritance".  A
hypergraph is a certain generalization of a graph that makes it simpler
to represent knowledge.  Note that hypergraphs can be represented with
ordinary graphs, and a specific representation, that of DAGs (directed
acyclic graphs) is used throughout the code.

Atoms come in two basic types: Nodes and Links.  Nodes correspond to the
leafs of a tree, while Links correspond to vertexes internal to the
tree (non-leaf vertexes).  Nodes can be given a name, Links cannot.
Nodes and Links have a type, but no further properties, except for
values (valuations); these are described in a later section.

Nodes and Links can be shared between different trees.  Nodes and Links
are, by definition, immutable: once created, they cannot be changed;
they can only be deleted.  Nodes and Links are, by definition, globally,
universally unique: there can only ever be one Node of a given type and
name. Likewise, there can only be one Link of a given type, holding the
Atoms that it does.  The uniqueness constraint is used to avoid
ambiguity and duplication in knowledge representation. The immutability
constraint is used to avoid race conditions, to prevent different
processes or users from accidentally seeing different versions of the
"same" thing.

By definition, Links contain (ordered) sets of Atoms: this is refered to
as the "outgoing set" of the Link. Conversely, an Atom might be
contained in one or more Links; these links form the "incoming set" of
the atom.

One way to understand typed hypergraphs is to realize that they are
"term graphs".  A formal definition of a "term graph" can be found on
page 66 of the book Franz Baader, Tobias Nipkow, "Term Rewriting and
All That", Cambridge University Press, 1998.  That book also provides
a general sketch of why, and how, term graphs can be used to represent
knowledge of any sort. Another way of looking at atoms is as the basic
building blocks of Model Theory: atoms are used to construct models.
See, for example, the book "A Shorter Model Theory", by Wilfred Hodges.

One way in which the concept of Atoms and AtomSpaces differ from
"standard" Model Theory or from "plain" term graphs is that the re-write
rules for term graphs can themselves be expressed in terms of atoms. So,
for example, the (typed) lambda calculus can be represented with atoms,
and beta-reduction is represented in terms of Atoms, as well. The
default defintions include several variants of beta reduction, including
the PutLink and the MapLink (which see, on the wiki).

What is the AtomSpace?
======================
The AtomSpace is a database that holds Atoms. In the default mode, the
AtomSpace is purely an in-RAM database, although the "backend API"
allows portions (or all) of an AtomSpace to be persisted to disk.

For some types of algorithms, it is convenient to have multiple
AtomSpaces; the implementation here supports this, although in a
relatively naive and unsophisticated way. The atomspaces can be nested;
one way to think of them is that they resemble the concept of an
"environment" in the scheme programming language (or the "environment"
in the bash shell).  Thus, the AtomSpace can be thought of as a space
holding "S-expressions"; indeed, every Link-Node tree (DAG) can be
thought of as an S-expression.

What is a Valuation?
====================
Although a typed hypergraph of Atoms can, in principle, represent any
kind of data, it is inefficient for many practical uses: the demand for
immutability and uniqueness implies a significant overhead for indexing
Atoms, and for thread-safety of the index.  Thus, for rapidly-changing
data, a different mechanism is provided: this associates one or more
"values" to an atom; the values are indexed by a "key" (which is an
Atom).  The values can be a vector of floats, a vector of strings, or a
vector of values; yet more kinds of values could be defined, if desired.

Values differ from Atoms in that they are not indexed in the AtomSpace,
which means that they cannot be searched-for by content. Unlike Atoms,
Values are also not globally unique: there may be two distinct Values
holding exactly the same data.  Values have no "incoming set": given
a Value, there is no way of finding out what Atoms might be using it.

Values, and in particular TruthValues, are immutable: once created,
they cannot be changed. What can be changed is the association of
Values to Atoms. Immutabilty seems to be a simpler, faster solution
than using mutex locks to guarantee consistent data. Immutability does
incurr some performance costs (for managing/swapping the smart
pointers, as well as forcing a malloc to change a value.).

The Valuation type is a C++ class that associates the (key,atom) pair
with a value; thus a Valuation is the triple (key, atom, value). Thus,
the Valuation loosely resembles the EvaluationLink, with the key playing
the role of a PredicateNode, and the value playing the role of the
ListLink.

The name "valuation" was chosen to explicitly call to mind the notion of
a "valuation" in model theory: so, for example, atoms are used to
represent sentences and theories, while valuations are used to assign
truth values to each atom. Indeed, the TruthValue is a special case of a
valuation. For historical reasons, the TruthValue is treated in a
special way in the implementation.

The ValuationSpace vaguely resembles the AtomSpace, and is used to solve
the technical issues that arise in managing valuations.


Atom Implementation Notes
=========================
The Link class explicitly contains the outgoing set of the Link.  The
Atom class explicitly contains the incoming set of the Atom. Smart
pointers (the C++ std::shared_ptr class) are used for holding Atoms.
The Handle class is a wrapper around std::shared_ptr.  The Link contains
an std::vector of Handles.  Thus, to avoid memory management issues, the
Atom uses weak pointers for the incoming set.

AtomSpace Implementation Notes
==============================
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

Valuation Implementation Nodes
==============================
Valuations are currently stored in a ValuationSpace, but it might be
better to store them directly, in each atom.

TruthValues are stored directly with each Atom; this could be changed
to use the generic Valuation mechanism, but this would incurr
significant(?) performance penalty.

Valuations are currently immutable, this too has performance and access
penalties that perhaps should be re-thought?


======================================================================
======================================================================
======================================================================

                      Adding atom types
                      -----------------

The ClassServer provides a primitive extension mechanism so that
modules/agents/libraries may add new atom types to the default type
hierarchy. In order to ease the task of third-parties wishing to extend
the ClassServer, we provide a cmake macro that generates a set of
files with c++ code that can be used by the module/library.

The macro uses a 'type script' file as input which uses the following
format:

<TYPE> [<- <PARENT_TYPE1>[,<PARENT_TYPE2>,<PARENT_TYPE3>,...]] ["<TYPE_NAME>"]

Where

    TYPE is an identifier that will be used in your code to reference
    the type's numeric code. Usually, it is defined using capital
    letters and underscores as its semantics is close to that of C/C++
    constant.

    PARENT_TYPE1, PARENT_TYPE1, PARENT_TYPE2 are optional identifiers of
    the parent types of the defined type. When more than one parent type
    is specified, they must be separated by commas.

    TYPE_NAME is a string that will be used to identify the type. If
    none is supplied, the cmake macro will generate one based on the
    type's identifer using camel-casing patterns (for instance,
    CUSTOM_NODE would be named "CustomNode").

Above is a short snippet of valid script entries. For more examples,
check the atom_types.script file.

ATOM
NODE <- ATOM
LINK <- ATOM
WORD_NODE <- NODE
CONCEPT_NODE <- NODE "OddlyNamedNode"
ASSOCIATIVE_LINK <- LINK "AssocL"
EVALUATION_LINK <- LINK "EvalLink"
MULTIPARENT_LINK <- ASSOCIATIVE_LINK_LINK,EVALUATION_LINK "MPLink"

-----
To process the 'atom types' script file, one must add the macro
OPENCOG_ADD_ATOM_TYPES to the CMakeLists.txt and the header file to the
list of source files:

# CMakeList.txt
OPENCOG_ADD_ATOM_TYPES(atom_types.script atom_types.h atom_types.definitions atom_types.inheritance)

ADD_LIBRARY(sample
    atom_types.h
    Sample1.cc
    Sample2.cc
    ...
)

The macro OPENCOG_ADD_ATOM_TYPES expects 4 parameters:

    1. the filename of the script file that will be used as input
    2. the filename of the header file that will be generated with
       the identifiers of the new atom types.
    3. the filename of the definitions file that will be generated with
       the instantiations of the variables that will store the new atom
       types.
    4. the filename of the inheritance file that will be generated with
       the set of method invocations that will build the type hierarchy
       inside the ClassServer.

To properly *use* the generated files, the following conventions should be
followed:

  * include the definitions file right after the standard '#include'
    statement of the file with the code that initializes your module/agent/library.

  * include the inheritance file *inside the body* of the routine
    initializing the module/agent/library.

  * include the header file by any files that references the identifier
    of the a new atom type.

For instance:

// MyModule.cc
#include "MyModule.h"
#include "AnotherHeader.h"
#include "atom_types.definitions"
MyModule::MyModule() {}
...
static __attribute__ ((constructor)) void _init(void)
{
    #include "atom_types.inheritance"
}


// AnotherFile.cc
#include "AnotherFile.h"
#include "atom_types.h"
#include "YetAnotherFile.h"

void AnotherFile::someMethod() {
    ...
    std::string name = opencog::ClassServer::getTypeName(opencog::MYNODE);
    ...
}

-----
For a fully functional example, check the ''examples/atomtypes/' directory.


======================================================================
======================================================================
======================================================================

                   Garbage Collection Design
                   -------------------------

As of October/November 2013, Handles use std::shared_pointer to deal
with memory management, and so using GC is no longer urgent.  The
shared_pointers seem to work OK, for now.  Based on performance
measurements, however, shared pointers are 3x slower than GC could be
(compare the AtomTable results for April and November 2013 in the
opencog/benchmark/diary.txt file, which show a rough 3x slowdown.)

Without garbage collection (or smart pointers) it becomes unsafe to use
bare pointers in a multi-threaded environment.  With garbage collection
(or smart pointers), and a reasonably good design and coding style, the
use of bare pointers in a multi-threaded environment becomes safe. The
problem with smart pointers is that they are (1) generally slower, and
(2) use significantly more RAM (40 bytes per pointer, instead of 8 bytes,
as well as additional bytes in the object, for the counter itself.)

A prototype using BDW-GC was attempted in 2014, but was found to have
problems.  The main problem is that the std:: container classes are
just not freindly for garbage collection.  Consider, for example, the
std::set container. If it has N objects in it, it will have log_2(N)
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


======================================================================
======================================================================
======================================================================

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

======================================================================
======================================================================
