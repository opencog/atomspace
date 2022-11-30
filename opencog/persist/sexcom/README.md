S-Expression Command Dispatching
--------------------------------
S-expression commands, "sexpr comms" or just sexcoms, are a special
collection of UTF-8 strings that are interpreted as commands to do
something. They contain embedded Atomese in the form of s-expression
strings. These are decoded and sent through the command dispatcher.
At the "center" of each command is a StorageNode object which performs
the desired command. The results are then re-encoded as strings, and
returned to the caller.

These sexcoms are used by the CogServer, working together with the
`CogStorageNode`, to send Atoms and Values across the TCP/IP network.
This is how distributed storage is implemented for teh AtomSpace.




The CogServer provides a network API to send/receive Atoms over the
internet. The actual API is that of the StorageNode (see the wiki page
https://wiki.opencog.org/w/StorageNode for details.) The cogserver
supports the full `StorageNode` API, and it uses the code in this
directory to respond to the `CogStorageNode` commands.

The `CogStorageNode` uses a very special set of about 15 scheme
functions to do network I/O. Rather than flowing these through
guile, which has hefty enter/exit penalties, these "sexpr comms"
have been hard-coded in C++. These are implemented in `Commands.cc`
The goal is to avoid the overhead of entry/exit into guile. This works
because the cogserver is guaranteed to send only these commands, and no
others. This is not a general scheme interpreter.

Proxying
--------
The code here places the StorageNode Proxy subsystem at it's center.
That is, a sexcom is decoded

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
