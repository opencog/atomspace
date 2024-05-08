Storage Proxies
---------------
Network-distributed AtomSpaces need to have proxy agents that know what
to do with the data being passed around.  Besides just working with the
attached AtomSpace, maybe something more needs to be done: maybe data
needs to be written to or read from disk. The StorageNode Proxy determines
what is to be done.

Example
--------
In the standard mode, when a network connection is made to the
CogServer, the user at the far end of the network connection is working
with the AtomSpace that this CogServer holds. It is reasonable to want
to have the CogServer attached to storage (say, disk storage), so that
when an Atom is received, it is also written to disk.  This general
idea is called "proxying", and "proxy agents" are responsible to doing
whatever needs to be done, when a read or write request for an Atom is
received by the CogServer. The proxy agent passes on those requests to
other StorageNodes.

The diagram below shows a typical usage scenario. In this example,
the Link Grammar parser is reading from the local AtomSpace to which
it is attached. That AtomSpace uses a CogStorageNode to get access
to the "actual data", residing on the CogServer. However, the CogServer
itself doesn't "have the data"; its actually on disk (in the
RocksStorageNode) Thus, any read requests made by the the LG parser
have to be proxied by the CogServer to the disk storage (using a
read-thru proxy.)
```
                                            +----------------+
                                            |  Link Grammar  |
                                            |    parser      |
                                            +----------------+
                                            |   AtomSpace    |
    +-------------+                         +----------------+
    |             |                         |                |
    |  CogServer  | <<==== Internet ====>>  | CogStorageNode |
    |             |                         |                |
    +-------------+                         +----------------+
    |  AtomSpace  |
    +-------------+
    |    Rocks    |
    | StorageNode |
    +-------------+
    |   RocksDB   |
    +-------------+
    | disk drive  |
    +-------------+
```

See the [proxy example](../../../examples/atomsppace/persist-proxy,scm)
in the examples directory for a working example.

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
***Version 0.9.1*** -- Seems to work. Has not been stressed.

Some open TODO items:

 * Move ProxyParametersLink from being a DefineLink to a StateLink.
   DefineLinks are a freakin pain in the neck. However, they do force
   the user to not screw around...

 * Multi-atomspace (frame) support is missing. Some basic work
   in this direction has been done, but it is not been completed.  The
   `SpaceFrame`, `SpaceDiamond`, `SpaceWye` and `FrameDelete` unit tests
   in the `atomspace-cog` git repo do not run/do not pass. Getting those
   to run and pass requires additional work to be done here (probably in
   the [sexpr-commands](../sexcom) directory.

 * The CachingProxy needs to get bells-n-whistles added. For example:
   the ability to limit to a max AtomSpace size, and the ability to
   expire old/stale data. This would allow the cache to be used with
   HUGE disk DB's, without the risk of running out of RAM.

 * Create a "Remembering Agent", which would be like the "CachingProxy
   in reverse" -- once RAM usage got too large, Atoms would be
   automatically saved to disk (and deleted from RAM). Similarly, it
   could back a large "live" AtomSpace, periodically writing to disk,
   so that the active user of the AtomSpace wouldn't need to concern
   themselves with need to manually store data. The tricky part with
   remembering, though, is avoiding accidental saves of temporary/junk
   data.

### Some RememberingAgent notes:
Designing an effective RememberingAgent is surprisingly difficult.

Here are some thoughts about such a design. One could stick a timestamp
on each Atom (a timestamp Value) and store the oldest ones. But this
eats up RAM, to store the timestamp, and then eats more RAM to keep
a sorted list of the oldest ones. If we don't keep a sorted list,
then we have to search the atomspace for old ones, and that eats CPU.
Yuck and yuck.

Every time a client asks for an Atom, we have to update the timestamp
(like the access timestamp on a Unix file.)  So, Unix files have three
timestamps to aid in decision-making - "created", "modified" and "accessed".
This works because most Unix files are huge compared to the size of the
timestamps. For the AtomSpace, the Atoms are the same size as the
timestamps, so we have to be careful not to mandate that every Atom
must have some meta-data.

There's another problem. Suppose some client asks for the incoming set
of some Atom. Well, is that in RAM already, or is it on disk, and needs
to be fetched? The worst-case scenario is to assume it's not in RAM, and
always re-fetch from disk. But this hurts performance. (what's the point
of RAM, if we're always going to disk???) Can one be more clever? How?

There's a third problem: "vital" data vs "re-creatable" data. For example,
a genomics dataset itself is "vital" in that if you erase anything, it's a
permanent "dataloss".  The [MOZI genomics code-base](https://github/mozi-ai),
as it is being used, performs searches, and places the search results into
the AtomSpace, as a cache, to avoid re-searching next time. These search
results are "re-creatable".   Should re-creatable data be saved to disk?
Sometimes? Always? Never? If one has a dozen Values attached to some Atom,
how can you tell which of these Values are "vital", and which are
"re-creatable"?

The above sketches three different problems that plague any designs
beyond the very simplest ones.  The obvious solutions are not very good,
the good solutions are not obvious.
