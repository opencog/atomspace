
AtomSpace Source Code
=====================


Directory overview
------------------
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
-----------------------
Please refer to the following for sepcific questions:

* Atom vs. Value Design tradeoffs/justification. See
 [atoms/proto/README.md](atoms/proto/README.md)

* How to add new atom or value types. See
 [README-Adding-New-Atom-Types.md](atoms/proto/README-Adding-New-Atom-Types.md)

* Performance benchmarks are no longer in this repo. See the
  [opencog/benchmark](https://github.com/opencog/benchmark) repo.
