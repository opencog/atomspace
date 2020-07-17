Fast Load of Atomese
--------------------
Load a file containing Atomese into the AtomSpace.  Atomese consists of
all of the s-expressions that define Atoms and (Truth-)Values. This is
"fast", because it is nearly an order of magnitude (10x) faster than
passing the same file through the scheme/guile interfaces.

The file can ***only*** contain Atomese s-expressions. No other subset
of scheme is supported!

To use: there is a python API. The scheme API is pending.
