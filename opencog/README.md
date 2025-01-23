
AtomSpace Source Code
=====================
The AtomSpace implementation is here. But first, a quick overview of the
basics.


What is an Atom?
----------------
An Atom is an immutable data structure that can be used to represent
knowledge. Atoms are designed to be so general that any kind of domain
knowledge can be represented with them, including classical predicate
logic, natural language, Bayesian probability networks, neural networks,
and so on. The basic representation is that of "typed hypergraphs". Each
Atom has a "type" (in the sense of "type theory") -- there are currently
over one hundred pre-defined types, such as "and", "or", "not", but also
"word", "sentence", as well as "implication" and "inheritance".  A
hypergraph is a certain generalization of a graph that makes it simpler
to represent knowledge.  Hypergraphs can be represented with ordinary
graphs, but are much easier to use (and are much faster.)

Atoms come in two basic types: Nodes and Links.  Nodes correspond to the
leaves of a tree, while Links correspond to vertecies internal to the
tree (non-leaf vertecies).  Nodes can be given a name, Links cannot.
Nodes and Links have a type, but no further properties, except for
Values (valuations); these are key-value databases described below.

Nodes and Links can be, and typically are shared by multiple different
trees.  Nodes and Links are, by definition, immutable: once created,
they cannot be changed; they can only be deleted.  Nodes and Links are,
by definition, globally, universally unique: there can only ever be one
Node of a given type and name. Likewise, there can only be one Link of
a given type, holding the Atoms that it does.  The uniqueness constraint
is used to avoid ambiguity and duplication in knowledge representation.
The immutability constraint is used to avoid race conditions, to prevent
different processes or users from accidentally seeing different versions
of the "same" thing.

By definition, Links contain (ordered) sets of Atoms: this is referred to
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
rules for term graphs can themselves be expressed in terms of Atoms. So,
for example, the (typed) lambda calculus can be represented with Atoms,
and beta-reduction is represented in terms of Atoms, as well. The
default definitions include several variants of beta reduction, including
the PutLink and the MapLink (which see, on the wiki).


What is a Valuation?
--------------------
Although a typed hypergraph of Atoms can, in principle, represent any
kind of data, it can sometimes be inefficient for certain practical
uses. The demand for immutability and uniqueness implies a significant
overhead: uniqueness requires indexing, with thread-safety (locking) on
index.  Thus, for rapidly-changing data, a different mechanism is
provided: the 'Value' system. Values are not unique, and they are
mutable. Arbitrary Values can be attached to Atoms; the attachment point
is indicated with a 'key' (which is necessarily an Atom.) Thus, every
Atom is (has, contains) a full-fledged key-value database.

Basic Value types include vectors of floats, strings and other Values.
Some Values hold references to external entities or data streams: think
flowing, live dynamic audio or video steams. Touching ('updating') the
value then just samples from that stream (depending on the
implementation.)

Values differ from Atoms in that they are not indexed in the AtomSpace,
which means that they cannot be searched-for by content. Of course, you
can brute-force traverse all of them, but that would be ... dumb.
Unlike Atoms, Values are not globally unique: there may be two distinct
Values holding exactly the same data.  Values have no "incoming set":
given a Value, there is no way of finding out who else is pointing at
it or using it. If you need to find all users/references, use an Atom.
That's what they are for.

Some Values are immutable; others are not. For historical reasons,
TruthValues are immutable; this was used to guaranteed a single view
of a Truth value to multiple observrs looking at the same time. That
is, TruthVales were designed tto be race-free.

Why immutabilty? Immutabile objects provide a simple, fast guarantee
of consistent data views, avoiding the overhead of using mutex locks.
There is a price, though: smart pointers must be used, and changing a
value does result in a malloc/free cycle. Note that C++ smart pointers
are atomic, and thus necessarily use the lock primitives deep inside of
CPU cache lines. Note that some low-end consumer grade CPUs don't have
enough of these (while others do), and sometimes high-end server CPUs
have a problem in this area as well. This is all highly proprietary,
the CPU vendors do not disclose this info. Measure twice, cut once.

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

Most ordinary graph DB's have a concept similar to Values: they
typically allow the user to store arbitrary data on each vertex
or edge of a graph.  That is, most graph DB's have a key-value
DB for each vertex/edge. Same idea here.


What is the AtomSpace?
----------------------
The AtomSpace is a database that holds Atoms. By default mode, the
AtomSpace is an in-RAM database. Various different
[StorageNodes](https://wiki.opencog.org/w/StorageNode) allow AtomSpace
contents to be stored to disk, to databases, and passed around on the
network. The [ProxyNodes](https://wiki.opencog.org/w/ProxyNode) provides
more sophisticated ways of moving Atoms around from here to there.

The AtomSpace does not provide any security or authentication mechanisms.
These must be supplied by external systems. It does provide a way of
marking an AtomSpace as being 'read-only', but this is provided as a
convenience, and not as a security mechanism.

For some types of algorithms, it is convenient to have multiple
AtomSpaces; for example, layering a smaller read-write atomspace
on top of a much larger read-only atomspace. Nested/overlayed atomspaces
behave somewhat like the concept of an "environment" in computer science.
The multi-atomspace concept is refered to as 'Frames' or sometimes
'SpaceFrames'. Frames allow not just layering, but also the creation of
unions of subspaces, as well as selective erasure, so that Atoms can be
"erased" (hidden, actually) in higher layers, while being alive an
healthy in lower layers.


What is the Query Engine?
-------------------------
The query engine, sometimes called the 'pattern matcher', allows rapid
search and discovery of AtomSpace contents that match a given template.
It implements a (hyper-)graph query language. A graph template can be
imagined as a graph with "holes" or "blanks" in it; the query engine
will "fill in the blanks".  The query engine has many advanced
capabilities, including automatically joining large, complex queries,
enabling graph re-writing, and executing triggers.

(The name 'pattern matcher' is often used, but is an unfortunate choice.
Many functional programming languages have "pattern matchers", but these
implement only a miniscule fragment of what the Atomese query engine can
do. The Atomese query engine is more like SQL, but with stuff that SQL
doesn't have/can't do.)

The query language is itself expressed as Atoms, and so the queries
themselves are stored in the AtomSpace. This enables things like a
"dual search": given a graph, find all queries that would match that
graph. This allows the database to act as a rule engine, holding a large
number of rules.

Examples of "dual searches" include chatbots, which, given some input
text, wish to locate possible replies by matching fragments of that
input. The pattern matcher extends the idea of this kind of search to
arbitrary graph structures.

What is the Matrix API?
-----------------------
(The matrix API has been moved to its own repo. It is now at
https://github.com/opencog/matrix)

Binary relations between things, for example "A is-a B", can be thought
of as defining a (sparse) matrix between things of type A and type B,
where A and B are individually the row and column labels in that matrix.
The value system allows arbitrary data to be stored at these matrix
locations, such as counts, frequencies, probabilities, and so on. This
in turn allows conditional probabilities, marginal probabilities and
other statistical properties, or linear-algebra in general to be
computed and stored.

This differs from traditional science packages, such as SciPy or Gnu R
(or Octave, or MatLab ...) in that the AtomSpace enables extremely sparse
matrices to be stored. For example, matrices of 200K by 200K entries
are not uncommon in linguistics and genomics/proteomics.  Non-sparse
representations would require 200K x 200K = 40 billion entries, the vast
majority of which are zero. The AtomSpace allows for ultra-sparse,
ultra-high-dimension matrixes to be stored.

This should also be contrasted to artificial neural nets and deep
learning: in most common applications of neural nets, the dimensions
of the "hidden layers" rarely exceed several thousand, precisely due
to O(N^2) explosion of non-zero entries. In all of the standard
approaches, the weight vectors in neural nets are NOT sparse. By
contrast, the AtomSpace provides a framework for performing neural-net
type research with ultra-high-dimension, ultra-sparse connectivity
diagrams.


Directory overview
==================
This directory contains core AtomSpace code.  Unit tests are in the
[tests](../tests) directory, and example and demo programs are in the
[examples](../examples) directory. Here's a short description of the
important subdirectories:

<dl>
<dt>atoms/atom_types<dd>Defines the atom type hierarchy.

<dt>atoms/value<dd>Defines Values; these are the base class for Atoms.

<dt>atoms/base <dd>Defines the basic atoms: Nodes and Links.

<dt>atoms/core <dd>Assorted special-case atoms, defined as C++ classes.
                   These typically cache some special information,
                   or have "imperative" methods that do special things,
                   when called.

<dt>atomspace  <dd>The in-RAM database or "symbol table" that holds
                   atoms. It assures that only one version of any
                   given atom can ever be found in an AtomSpace.

<dt>query      <dd>Pattern matching for the atomspace. Allows for
                   specific patterns to be extracted from the atomspace.
                   Like SQL, but for graphs, instead of tables.

<dt>sheaf      <dd>Papers and documentation describing a sheaf-theoretic
                   approach to parsing, inference and theorem-proving.

<dt>guile, scm <dd>Scheme language bindings.
<dt>cython     <dd>Python language bindings.
<dt>haskell    <dd>Haskell language bindings.
<dt>ocaml      <dd>OCaML language bindings.

</dl>

Important Documentation
=======================
Please refer to the following for specific questions:

* Atom vs. Value Design tradeoffs/justification. See
  [atoms/proto/README.md](atoms/value/README.md)

* Atomspace design tradeoffs, including commentary about memory
  management, multi-threading, overlay atomspaces and more, are
  discussed in the [atomspace/README.md](atomspace/README.md) file.

* How to add new atom or value types. See
  [README-Adding-New-Atom-Types.md](atoms/atom_types/README-Adding-New-Atom-Types.md)

* Performance benchmarks are no longer in this repo. See the
  [opencog/benchmark](https://github.com/opencog/benchmark) repo.
