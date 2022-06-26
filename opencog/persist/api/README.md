StorageNode API
---------------
Provides a C++ interface for access to (disk, network) AtomSpace storage.

### TODO
* Add a read-only flag, to see if the storage node is not writable.
  (This is a small task. Just do it.) Users, such as the write-thru
  proxy, want to know if a StorageNode is read-only, and so can avoid
  sending it writes.

* Maybe all space-frame support should be handled by the StorageNode?
  Right now, space-frame support is ad-hoc, in `Commands.cc` and in
  to RocksStorageNode. It probably needs to be generic!?

Persist Guile API
-----------------
Provides a scheme module for the generic persistence API.

To use:
```
(use-modules (opencog persist))
```
The provided functions are:

* `fetch-atom ATOM` --
      Get the current Values (including TruthValues) on `ATOM`.
* `fetch-value ATOM KEY` --
      Get the current Value on `ATOM` at `KEY`.
* `fetch-incoming-set ATOM` --
      Get all of the Atoms that contain `ATOM`.
* `fetch-incoming-by-type ATOM TYPE` --
      Get all Atoms of type TYPE that contain `ATOM`.
* `fetch-query QUERY KEY META FRESH` --
      Perform the `QUERY`, place results at `KEY` and metadata at `META`.
      An earlier cached query may be returned unless `FRESH` is true.
* `load-atoms-of-type TYPE` --
      Get all Atoms of type TYPE.
* `load-atomspace` --
      Get all Atoms (in the peristant store).
* `store-atom ATOM` --
      Put the `ATOM` and all attached Values into the persistent store.
* `store-value ATOM KEY` --
      Put the Value located at `KEY` on `ATOM` into the persistent store.
* `store-atomspace` --
      Put all of the Atoms into the persistent store.
* `barrier` --
      Complete any async, pending load/store operations before
      continuing with the next load/store operation.

Recall that you can always get more information and documentation with
the `,a` `,apropos` `,d` and `,describe` commands. For example, saying
`,a fetch` will list all commands with `fetch` in their name, and 
`,d fetch-incoming-set` will print the full documentation.
