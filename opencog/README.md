
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
values (valuations); these are key-value databases described in a
section below.

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
kind of data, it is inefficient for many practical uses: the demand for
immutability and uniqueness implies a significant overhead for indexing
Atoms, and for thread-safety of the index.  Thus, for rapidly-changing
data, a different mechanism is provided: this associates one or more
"values" to an atom; the values are indexed by a "key" (which is an
Atom).  The values can be a vector of floats, a vector of strings, or a
vector of values. They can also be dynamically time-varying, such as
audio or video streams. Yet more kinds of values could be defined, if
desired.

Values differ from Atoms in that they are not indexed in the AtomSpace,
which means that they cannot be searched-for by content. Unlike Atoms,
Values are also not globally unique: there may be two distinct Values
holding exactly the same data.  Values have no "incoming set": given
a Value, there is no way of finding out what Atoms might be using it.

Values, and in particular TruthValues, are immutable: once created,
they cannot be changed (unless they are dynamic, streaming values,
which, by definition, are ever-changing.)  What can be altered is the
association of Values to Atoms.

Why were values made immutable? Immutability seems to be a simpler,
faster solution than using mutex locks to guarantee consistent data.
Immutability does incur some performance costs (for managing/swapping
the smart pointers, as well as forcing a malloc to change a value.).

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

Most ordinary graph DB's have a concept very similar to Values: they
typically allow the user to store arbitrary data associated with each
vertex or edge of a graph.  That is, must graph DB's have a key-value
DB for each vertex/edge. Same idea here.


What is the AtomSpace?
----------------------
The AtomSpace is a database that holds Atoms. In the default mode, the
AtomSpace is purely an in-RAM database, although the "backend API"
allows portions (or all) of an AtomSpace to be persisted to an SQL
DB server, or (unimplemented) various different graph DB stores.
Currently, only the PostgresSQL backend works. The Neo4j backend is
borken.

The AtomSpace is distributed, and can be shared by different
network-connected machines. It relies on the backend (e.g. Postgres)
to do the hard work of actually distributing the data.  The AtomSpace
acts like an in-RAM local cache; holding only those parts of the
dataset that you are currently operating on.

All security, access-control-lists, etc. is managed by the backend DB.
The AtomSpace itself does not provide any use or access models.

For some types of algorithms, it is convenient to have multiple
AtomSpaces; for example, layering a smaller read-write atomspace
on top of a much larger read-only atomspace. Nested/overlayed atomspaces
behave somewhat like the concept of an "environment" in computer science.
The current multi-atomspace/nesting implementation is "beta"; its used
heavily by the pattern matcher, but remains a bit naive in some ways.


What is the Pattern Matcher?
----------------------------
The pattern-matcher is the query engine that allows the contents of the
atomspace to be located and traversed. It implements a kind of graph
query language. In essence, you can specify a graph with "holes" or
"blanks" in it; the query engine will "fill in the blanks".  The query
engine has many advanced capabilities, including automatically joining
large, complex queries, enabling graph re-writing, and executing
triggers.

The query language is itself expressed as Atoms, and so the queries
themselves are stored in the atomspace. This enables things like a
"dual search": given a graph, find all queries that would match that
graph. This allows the database to act as a rule engine, holding a large
number of rules.

Examples of "dual searches" include chatbots, which, given some input
text, wish to locate possible replies by matching fragments of that
input. The pattern matcher extends the idea of this kind of search to
arbitrary graph structures.

What is the Matrix API?
-----------------------
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


What is the Unified Rule Engine?
--------------------------------
A single graph query (pattern-match query) can be thought of as a single
graph re-write step: when a graph matches the query, the shape of the
graph is altered (edited) as a result. Thus, each query can be thought
of as a "rule", to be applied to the data. If the rule triggers, then
it runs and transforms the data.  The Unified Rule Engine (URE) is the
infrastructure that allows collections of rules to be specified, and
manage their application.

There are four different ways of managing rules. Two are the traditional
forward-chaining and backward-chaining algorithms, full analogous to
those chainers commonly seen in many kinds of logic engines and theorem
proving systems (most rule engines provide only a forward chainer).
Its not in this repo; it's in the
[opencog/ure](https://github.com/opencog/ure) repo, and is at release
level, and fully supported.

A third type of rule engine is based on the idea of parsing. It is
neither a forward nor backward chainer, but combines a bit of both,
and behaves like a parser. The current implementation is
super-experimental, pre-prototype, in the "sheaf" directory.

A fourth type of rule engine is the so called "openpsi" system. It is
goal-driven in its rule selection, and is modulated and prioritized
by what is considered to be "important" at the moment. It resembles
a kind-of planning system. Its not in this repo; it's in the
[opencog/opencog](https://github.com/opencog/opencog) repo, and is
in an experimental-prototype stage.


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

<dt>persist    <dd>Methods for communication between servers, also,
                   saving/restoring the AtomSpace to disk/databases.

<dt>query      <dd>Pattern matching for the atomspace. Allows for
                   specific patterns to be extracted from the atomspace.
                   Like SQL, but for graphs, instead of tables.

<dt>matrix     <dd>Present a view of a subset of the atomspace as a
                   (sparse) matrix, *e.g.* a covariance/correlation
                   matrix, allowing statistical matrix analysis
                   (PCA, SVD, etc.) to be performed on this subset.

<dt>object-atomese  <dd>Proposal for object-oriention in Atomese.
                    Goal is to allow porting of the matrix API to pure
                    Atomese.

<dt>sheaf      <dd>Infer the grammar of a (hidden) dynamic network, by
                   means of sections of sheaves. Intended for generic
                   time series, e.g. natural language.  Currently
                   implements a Maximum Spanning Tree (MST) parser.

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
