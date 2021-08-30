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
;
; This demo shows how to dump and read in the entire AtomSpace, and how
; to store individual Atoms and Values. The `persistance.scm` demo shows
; how to use additional `StorageNode` API interfaces to read individual
; Atoms and perform queries. (The flat-file API does not support reading
; individual Atoms.)
; ---------------------------------------------------------------------

(use-modules (opencog) (opencog persist) (opencog persist-file))

; Populate the AtomSpace with some data.
(define a (Concept "foo"))
(cog-set-value! a (Predicate "num") (FloatValue 1 2 3))
(cog-set-value! a (Predicate "str") (StringValue "p" "q" "r"))

(define li (Link (Concept "foo") (Concept "bar")))
(cog-set-value! li (Predicate "num") (FloatValue 4 5 6))
(cog-set-value! li (Predicate "str") (StringValue "x" "y" "z"))

; Store some individual Atoms, and then store everything.
(define fsn (FileStorageNode "/tmp/foo.scm"))
(cog-open fsn)

; Store just one Atom.
(store-atom a fsn)

; Store just one value on an Atom. Store it three times;
; it will show up in the file three times.
(store-value a (Predicate "num") fsn)
(store-value a (Predicate "num") fsn)
(store-value a (Predicate "num") fsn)

; The file write might not occur until after the `barrier` call.
; File writes are buffered by the operating system.
(barrier fsn)

; Now store everything. This will appear after the writes above,
; and will duplicate some of the earlier data.  During file read,
; later data in the file will take precedence over earlier data.
(store-atomspace fsn)
(cog-close fsn)

; ---------------------------------------------------------------------
; At this point of the demo, it would be best to exit guile, and take
; a look at the contents of `/tmp/foo.scm`. It should be obvious.
; After exiting and restarting guile, continue with the below.
; ---------------------------------------------------------------------

(use-modules (opencog) (opencog persist) (opencog persist-file))

; Load everything from the file.
(define fsn (FileStorageNode "/tmp/foo.scm"))
(cog-open fsn)
(load-atomspace fsn)
(cog-close fsn)

; Verify the load
(cog-prt-atomspace)

; Verify that the keys on the link have been correctly restored.
(define li (Link (Concept "foo") (Concept "bar")))
(cog-keys li)
(cog-value li (Predicate "num"))
(cog-value li (Predicate "str"))

; --------------------------
; The End. That's all folks!
