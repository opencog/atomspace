
Demo of various StorageNodes
============================
This directory contains examples that show how Atoms and AtomSpaces can
be moved around between (tcp/ip) network nodes,  and saved/restored to
disk.

The prefered disk storage backend is `RocksStorageNode`, which uses
RocksDB to store Atoms and AtomSpaces. The preference network interface
is `CogStorageNode`, which uses TCP/IP to communicate with remote 
`CogServerNode`s "out in the cloud".

* [`persist-file.scm`](persist-file.scm)      -- Dump and load Atoms to a plain-text file.
* [`persist-store.scm`](persist-store.scm)    -- StorageNode API to a plain-text file.
* [`persistence.scm`](persistence.scm)        -- StorageNode API to an SQL database.
* [`persist-query.scm`](persist-query.scm)    -- Fetching sets of Atoms with queries.
* [`persist-multi.scm`](persist-multi.scm)    -- Work with multiple databases/servers at once.
* [`persist-proxy.scm`](persist-proxy.scm)    -- Work with proxy agents to access multiple dbs.
* [`persist-buffer.scm`](persist-buffer.scm)  -- Delay repeated writes so as to limit I/O.


All of these examples require the assorted storage-related modules to be
compiled and installed.  They can be obtained via `git clone` of

   https://gitub.com/opencog/atomspace-storage
   https://gitub.com/opencog/atomspace-rocks
   https://gitub.com/opencog/cogserver
   https://gitub.com/opencog/atomspace-cog

The build and install steps are identical to those for the AtomSpace.
The `atomspace-storage` component provides the generic network and file
system API, while `atomspace-rocks` adapts it for the RocksDB database.
The `cogserver` repo has code for the `CogServerNode`. This is an `Atom`
(a real, bona-fide, full-fledged `Node`) with some complex network
capabilities built in. The `atomspace-cog` repo provides the
`CogStorageNode`, which implements the ~StorageNode` API for network
communications.
