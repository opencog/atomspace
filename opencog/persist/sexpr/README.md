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

Only Atomese s-expressions, plus a very special subset of other
functions are supported. This is NOT a generic scheme interpreter.

Network API and Proxying
------------------------
The CogServer provides a network API to send/receive Atoms over the
internet. The actual API is that of the StorageNode (see the wiki page
https://wiki.opencog.org/w/StorageNode for details.) The cogserver
supports the full `StorageNode` API, and it uses the code in this
directory in order to make it fast.

To aid in performance, a very special set of about 15 scheme functions
have been hard-coded in C++. These are implemented in `Commands.cc`
The goal is to avoid the overhead of entry/exit into guile. This works
because the cogserver is guaranteed to send only these commands, and no
others.

Network-distributed AtomSpaces need to have proxy agents that know what
to do with the data being passed around.  Besides just working with the
attached AtomSpace, maybe something more needs to be done: maybe data
needs to be written to or read from disk. The StorageNode Proxy determines
what is to be done.

String to Callback Map
----------------------
Below is a short table summarizing the mapping of network strings to
the StorageNode API. When a network string is received (by `Commands.cc`)
it is decoded, and a corresponding StorageNode API call is made. For
example, the network string `(cog-incoming-set ...)` results in a call
to `Commands::cog_incoming_set(...)` (note how the dashes are now
underbars) and then `StorageNode::fetch_incoming_set()` is called.
Many calls require a call to `StorageNode::barrier()`.

The single arrow `->` is the string to `StorageNode` map. The double
arrow `=>>` is the `StorageNode` to `BackingStore` map.

```
TBD:
cog_atomspace
cog_atomspace_clear
cog_set_proxy
cog_execute_cache <== probably obsoleted by proxy !?

ReadThru:
cog_get_atoms -> fetch_all_atoms_of_type+barrier =>> loadType
cog_incoming_by_type -> fetch_incoming_by_type+barrier =>> fetchIncomingByType
cog_incoming_set -> fetch_incoming_set+barrier =>> fetchIncomingSet
cog_keys_alist -> fetch_atom+barrier =>> getAtom
cog_node -> ??? fetch_atom+barrier
cog_link -> ??? fetch_atom+barrier
cog_value -> fetch_value+barrier =>> loadValue

WriteThru:
cog_extract -> remove_atom =>> removeAtom
cog_extract_recursive -> remove_atom
cog_set_value -> store_value =>> storeValue
cog_set_values -> store_atom =>> storeAtom
cog_set_tv -> store_value =>> storeValue
cog_update_value -> update_value =>> updateValue

Misc:
cog_define
```


Status & TODO
-------------
***Version 1.0.2*** -- Everything works, has withstood the test of time.
***Version 1.1.0*** -- Under construction Proxy redesign.

However -- multi-atomspace (frame) support is missing. Some basic work
in this direction has been done, but it is not been completed.  The
`SpaceFrame`, `SpaceDiamond`, `SpaceWye` and `FrameDelete` unit tests
in the `atomspace-cog` git repo do not run/do not pass. Getting those
to run and pass requires additional work to be done here.
