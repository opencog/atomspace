S-Expression Command Dispatching
--------------------------------
S-expression commands, "sexpr comms" or just sexcoms, are a special
collection of UTF-8 strings that are interpreted as commands to do
something. They contain embedded Atomese in the form of s-expression
strings. These are decoded and sent through the command dispatcher.
At the "center" of each command is a StorageNode object which performs
the desired command. The results are then re-encoded as strings, and
returned to the caller.

The "central" StorageNode allows custom handling to be chained in.
The primary use is for proxying, where the central StorageNode would
be a ReadThruProxy or a WriteThruProxy, or perhaps a combination of
others. The ReadThruProxy acts as an "agent": it fetches Atoms from
storage, placing them in the local AtomSpace. Because this is done
before the sexcom reply is generated, it allows the AtomSpace to be
dynamically updated.

If the central StorageNode is not set, all sexcoms workt directly with
the current AtomSpace (i.e. with whatever it currently contains).

These sexcoms are used by the CogServer, working together with the
`CogStorageNode`, to send Atoms and Values across the TCP/IP network.
This is how distributed storage is implemented for the AtomSpace.
The proxying allows Atom fetch or store commands arriving over the
network to be fetched from disk storage first (or otherwise manipulated
before a reply is given to the CogServer.

See the wiki page https://wiki.opencog.org/w/StorageNode for API
documentation.

These sexcoms are also understood by guile; there are AtomSpace
guile functions that have exactly the same name. These even work
the same way, when there is no proxying. The sexcoms do two things
that guile doesn't:

* They allow proxying.
* By being coded in straight C++, they avoid the rather hefty CPU
  penalty of entering/exiting the guile interpreter.


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
