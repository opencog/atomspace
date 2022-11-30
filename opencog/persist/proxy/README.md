Storage Proxies
---------------
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
