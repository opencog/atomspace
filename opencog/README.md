
AtomSpace Source Code
=====================
The atomspace implementation is here. But first, a quick overview of the
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
----------------------
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
--------------------
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


Directory overview
==================
This directory contains core AtomSpace code.  Unit tests are in the
[tests](../tests) directiory, and example and demo programs are in the
[examples](../examples) directory. Here's a short description of the
important subdirectories:

<dl>
<dt>atoms/proto<dd>Defines Values (the base class for Atoms). Defines
                   the atom type hierarchy.  Everything else depends
                   on this.

<dt>atoms/base <dd>Defines the basic atoms: nodes, links, and handles.

<dt>atoms/core <dd>Assorted special-case atoms, defined as C++ classes.
                   These typically cache some special information,
                   or have "imperative" methods that do special things,
                   when called.

<dt>atomspace  <dd>The in-RAM database or "symbol table" that holds
                   atoms. It assures that only one version of any
                   given atom can ever be found.

<dt>persist    <dd>Methods for communication between servers, also,
                   saving/restoring the atomspace to databases.

<dt>query      <dd>Pattern matching for the atomspace. Allows for
                   specific patterns to be extracted from the atomspace.
                   Like SQL, but for graphs, instead of tables.

<dt>rule-engine<dd>Apply arbitrary collections of rules to the atomspace.
                   Provides forward and backward chainers.

<dt>matrix     <dd>Present a view of a subset of the atomspace as a
                   (sparse) matrix, *e.g.* a covariance/correlation
                   matrix, allowing statistical matrix analysis
                   (PCA, SVD, etc.) to be performed on this subset.

<dt>sheaf      <dd>Infer the grammar of a (hidden) dynamic network, by
                   means of sections of sheaves. Intended for generic
                   time series, e.g. natural language.  Currently
                   implements a Maximum Spanning Tree (MST) parser.

<dt>guile, scm <dd>Scheme language bindings.
<dt>haskell    <dd>Haskell language bindings.
<dt>python,cython<dd>Python language bindings.

</dl>

Important Documentation
=======================
Please refer to the following for specific questions:

* Atom vs. Value Design tradeoffs/justification. See
 [atoms/proto/README.md](atoms/proto/README.md)

* Atomspace deisgn tradeoffs, including commentary about memory
  management, multi-threading, overlay atomspaces and more, are
  discussed in the [atomspace/README.md](atomspace/README.md) file.

* How to add new atom or value types. See
 [README-Adding-New-Atom-Types.md](atoms/proto/README-Adding-New-Atom-Types.md)

* Performance benchmarks are no longer in this repo. See the
  [opencog/benchmark](https://github.com/opencog/benchmark) repo.
