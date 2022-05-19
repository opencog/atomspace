;
; file-storage.scm -- Unit test for the FileStorageNode
;
; This is a modified copy of the `persist-store.scm` demo in the main
; examples directory.
;
(use-modules (opencog) (opencog persist) (opencog persist-file))
(use-modules (opencog test-runner))

; ---------------------------------------------------------------------
; Create a unique file name.
(set! *random-state* (random-state-from-platform))
(define fname (format #f "/tmp/opencog-test-~D.scm" (random 1000000000)))

(format #t "Using file ~A\n" fname)

; ---------------------------------------------------------------------
(opencog-test-runner)
(define tname "store_load_file")
(test-begin tname)

; Populate the AtomSpace with some data.
(define wa (Concept "foo"))
(cog-set-value! wa (Predicate "num") (FloatValue 1 2 3))
(cog-set-value! wa (Predicate "str") (StringValue "p" "q" "r"))

(define wli (Link (Concept "foo") (Concept "bar")))
(cog-set-value! wli (Predicate "num") (FloatValue 4 5 6))
(cog-set-value! wli (Predicate "str") (StringValue "x" "y" "z"))

; Store some individual Atoms, and then store everything.
(define wfsn (FileStorageNode fname))
(cog-open wfsn)

; Store just one Atom.
(store-atom wa wfsn)

; Store just one value on an Atom. Store it three times;
; it will show up in the file three times.
(cog-set-value! wa (Predicate "num") (FloatValue 11 22 33))
(store-value wa (Predicate "num") wfsn)
(store-value wa (Predicate "num") wfsn)
(store-value wa (Predicate "num") wfsn)

; The file write might not occur until after the `barrier` call.
; File writes are buffered by the operating system.
(barrier wfsn)

; Now store everything. This will appear after the writes above,
; and will duplicate some of the earlier data.  During file read,
; later data in the file will take precedence over earlier data.
(store-atomspace wfsn)
(cog-close wfsn)

(cog-atomspace-clear)

; ---------------------------------------------------------------------

; Load everything from the file.
(define rfsn (FileStorageNode fname))
(cog-open rfsn)
(load-atomspace rfsn)
(cog-close rfsn)

; Verify the load
(cog-prt-atomspace)

; Verify that the keys have been correctly restored.
(define ra (Concept "foo"))

(test-assert "Concept Keys" (equal? 2 (length (cog-keys ra))))
(test-assert "Concept Num"
	(equal? (cog-value ra (Predicate "num")) (FloatValue 11 22 33)))
(test-assert "Concept Str"
	(equal? (cog-value ra (Predicate "str")) (StringValue "p" "q" "r")))

(define rli (Link (Concept "foo") (Concept "bar")))
(test-assert "List Keys" (equal? 2 (length (cog-keys rli))))
(test-assert "List Num"
	(equal? (cog-value rli (Predicate "num")) (FloatValue 4 5 6)))
(test-assert "List Str"
	(equal? (cog-value rli (Predicate "str")) (StringValue "x" "y" "z")))

; --------------------------
; Clean up.
(delete-file fname)

(test-end tname)

(opencog-test-end)
