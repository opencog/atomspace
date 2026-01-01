
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

