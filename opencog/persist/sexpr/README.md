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

Only Atomese s-expressions are supported. This is NOT a generic scheme
interpreter.


Status & TODO
-------------
***Version 1.0.2*** -- Everything works, has withstood the test of time.

Except Frame support may be wobbly. Frames are new, and do not yet flow
properly through the s-expression commands subsystem.
