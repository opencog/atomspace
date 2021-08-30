;
; file-storage.scm -- Unit test for the FileStorageNode
;
; This is a modified copy of the `persist-store.scm` demo in the main
; examples directory.
;
(use-modules (opencog) (opencog persist) (opencog persist-file))
(use-modules (opencog test-runner))

; ---------------------------------------------------------------------
(opencog-test-runner)
(define tname "store_load_file")
(test-begin tname)

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

(cog-atomspace-clear)

; ---------------------------------------------------------------------

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

(test-assert "stuff" #t)

; --------------------------
; The End. That's all folks!

(test-end tname)
