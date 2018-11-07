
AtomSpace Source Code
=====================


Directory overview
------------------
This directory contains core AtomSpace code.  Unit tests are in the
[tests](../tests) directiory, and example and demo programs are in the
[examples](../examples) directory.

Source code overview:

<dl>
<dt>atoms/base <dd>Defines atoms, nodes, links, truth values, etc.
                   Everything else depends on this.

<dt>atoms/core <dd>Assorted special-case atoms, defined as C++ classes.
                   These typically cache some special information,
                   or have "imperative" methods that do special things,
                   when called.
</dl>

atomspace        | The in-RAM database or "symbol table" that holds
                   atoms. It assures that only one version of any
                   given atom can ever be found.

persist          | Methods for communication between servers, also,
                   saving/restoring the atomspace to databases.

query            | Pattern matching for the atomspace. Allows for
                   specific patterns to be extracted from the atomspace.
                   Like SQL, but for graphs, instead of tables.

rule-engine      | Apply arbitrary collections of rules to the atomspace.
                   Provides forward and backward chainers.

matrix           | Present a subset of the atomspace as a matrix, e.g.
                   a covariance/correlation matrix, allowing statistical
                   matrix analysis (PCA, SVD, etc.) to be performed on
                   this subset.

sheaf            | Infer the grammar of a (hidden) dynamic network, by
                   means of sections of sheaves. Intended for generic
                   time series, e.g. natural language.  Currently
                   implements a Maximum Spanning Tree (MST) parser.

guile, scm       | Scheme language bindings.
haskell          | Haskell language bindings.
python,cython    | Python language bindings.

benchmark        | Performance benchmarks.


Important Documentation
-----------------------
Please refer to the following for sepcific questions:

* Atom vs. Value Design tradeoffs/justification. See
 [atoms/proto/README.md](atoms/proto/README.md)

* How to add new atom or value types. See
 [README-Adding-New-Atom-Types.md](atoms/proto/README-Adding-New-Atom-Types.md)
