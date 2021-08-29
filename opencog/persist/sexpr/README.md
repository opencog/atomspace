Atomese S-Expressions
---------------------
Read and write UTF-8 Atomese s-expression strings. This is a collection
of utilities that take Atomese s-expressions and convert then into
in-RAM Atoms and Values living in an AtomSpace. It is intended to make
it easy and fast to serialize and deserialize AtomSpace contents, so
as to save Atomese in a file, or ship it over the network.

The code is meant to be "fast", because it is nearly an order of
magnitude (10x) faster than passing the same contents through the
scheme/guile interfaces. The speed is in fact roughy comparable to
working with Atoms directly, in that it takes about the same amount
of CPU time to string-compare, string-substring, string-increment,
as it does to create Atoms and place them in the AtomSpace. (Yes,
we measured. Results were a mostly-even 50-50 split.)

Currently, only Atomese s-expressions are supported. No other subset
of scheme is supported!

The code includes a file-reader utiliity.

C++ API
-------
The `fast_load.h` file defines the C++ API.

Python API
----------
There's a python wrapper for this, somewhere. Not sure where.

Scheme API
----------
The fast file loader loads Atomese formatted as scheme. So, of course,
you could just use the scheme `load` function!  But this can be slow,
painfully slow. The fast loader provides a 10x performance improvement.

Use it like so:
```
(use-modules (opencog))
(use-modules (opencog persist-file))

(load-file "/some/path/to/atomese.scm")
```

where `atomese.scm` contains Atomese. The contents of the AtomSpace can
be written out by saying `(export-all-atoms "/tmp/atomese.scm")`. The
`export-atoms`, `cog-prt-atomspace` and `prt-atom-list` are useful for
writing Atoms to a file.

Network API
-----------
The cogserver provides a network API to send/receive Atoms over the
internet. The actual API is that of the StorageNode (see the wiki page
https://wiki.opencog.org/w/StorageNode for details.) The cogserver
supports the full `StorageNode` API, and it uses the code in this
directory in order to make it fast.

To aid in performance, a very special set of about 15 scheme functions
have been hard-coded in C++. These are implemented in `Commands.cc`
The goal is to avoid the overhead of entry/exit into guile. This works
because the cogserver is guaranteed to send only these commands, and no
others.
