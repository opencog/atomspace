;
; persist-store.scm - Load and store plain s-expressions to/from a file.
;
; This demo illustrates the most basic usage of the StorageNode API.
; It is used to write individual Atoms, and entire AtomSpaces, to a
; flat ASCII (UTF-8) file, in a human-readable form, as s-expressions.
; The s-expressions are identical to the scheme format for Atomese:
; they are identical to what one would write at the guile interpreter
; prompt.  The file format itself is just the Atomese subset of scheme;
; this is because reading/writing Atomese directly is much faster (more
; than 10x faster) than working through guile.  This is no fault of
; guile; its simply a statement that the full expressive power of scheme
; is not needed to store all of the AtomSpace.
;
; The `FileStorageNode` implements a subset of the full `StorageNode` API.
; This is because flat files are not databases, and so the search and
; query portions of the `StorageNode` API are not supported.  If you
; want a full-featured file interface, use the `RocksStorageNode`.
