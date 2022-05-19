;
; persist-file.scm - Dump and read plain-text Atomese to/from a file.
;
; The AtomSpace is an in-RAM database. You just might want to sometimes
; write some of it out to disk, and save it for later; or maybe you want
; to share AtomSpace contents with other users. There are several ways
; to do this. The "best" way to do this is to use the RocksStorageNode.
; It is a full-fledged database (built on RocksDB) with all the database
; bells and whistles.  Unfortunately, it is not "human-readable"; the
; file format used by RocksDB is binary.
;
; This demo illustrates how to dump plain-text Atomese to a file, and
; several ways of reading it back in. The most important part of this
; demo is to illustrate the fast file reader, which is vital for loading
; large files quickly.
;
; The API below is the very simplest, brute-force mechanism for storing
; Atoms (but not Values) to a file.  A different and more flexibile
; interface is provided by `FileStorageNode`, which implements a subset
; of the `StorageNode` API suitable for flat files. See
; `persist-store.scm` for a working example.

(use-modules (ice-9 readline))
(activate-readline)

(use-modules (opencog))

; Place some atoms into the AtomSpace
(Concept "asdf" (stv 0.42 0.24))
(List (Concept "cat") (Concept "dog"))

; Take a look at what is in the AtomSpace
(cog-prt-atomspace)

; Write the entire contents of the AtomSpace to a file
(export-all-atoms "/tmp/x.scm")

; Erase the entire AtomSpace
(clear)

; Verify that the AtomSpace is empty.
(cog-prt-atomspace)

; Load the AtomSpace
(load "/tmp/x.scm")

; Verify that everything loaded.
(cog-prt-atomspace)

; During the loading, you should have seen something like this:
;
; ;;; note: auto-compilation is enabled, set GUILE_AUTO_COMPILE=0
; ;;;       or pass the --no-auto-compile argument to disable.
; ;;; compiling /tmp/x.scm
; ;;; compiled /home/foo/.cache/guile/ccache/3.0-LE-8-4.2/tmp/x.scm.go
;
; That is, the built-in scheme loader is *compiling* the data as it
; loads it. For plain Atomese, this is a complete waste of time, and,
; for large files, it can be painfully slow. Yes, you can disable it
; with the GUILE_AUTO_COMPILE flag, or by loading it "raw", but that
; still won't be enough for extrememly large files. One can do even
; better, by using the "fast loader".
;
; But first: you can load Atomese without compiling by doing this:
(primitive-load "/tmp/x.scm")

; To use the fast file loader, do this:
(use-modules (opencog persist-file))
(load-file "/tmp/x.scm")

; The `load-file` function is ten times faster than `primitive-load`.
; It is the best, fastest way to load pure Atomese.  However, the file
; contents must be pure Atomese; `load-file` cannot support generic
; scheme; only `load` and `primitive-load` do this.
;
; To write Atomese to a file, you can use
; * `export-all-atoms`  -- write the entire contents of the AtomSpace
; * `export-atoms`      -- write only the specified Atoms.
; * `prt-atom-list`     -- print the specified Atoms
; * `cog-prt-atomspace` -- print the entire AtomSpace
;
; For details on how to use these, use the ,describe function:

,d export-all-atoms
,describe export-atoms
,des prt-atom-list

;
; That's all for now.
