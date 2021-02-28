
Storage Atom Types
------------------
This defines some atom types, currently, just nodes, that indicate
how to connect up to storage. There are currently three types:

* `PostgresStorageNode` -- connect to a Postgres backend
* `RocksDBStorageNode` -- connect to a RocksDB backend
* `CogStorageNode` -- connect to a CogServer backend

`RocksDBStorageNode` is implemented in the
[opencog/atomspace-rocks](https://github.com/opncog/atomspace-rocks)
git repo.

`CogStorageNode` is implemented in the
[opencog/atomspace-cog](https://github.com/opncog/atomspace-cog)
git repo.

General information about the `storage_types.script` file can be found
in the `opencog/atoms/atom_types` directory.
