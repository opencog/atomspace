;
; bootstrap.scm -- starting a data analytics pipeline.
;
; A desirable compute task is to run a data analytics pipeline, written
; in Atomese, to examine the contents of some "other" AtomSpace,
; containing some sort of data to be analyzed (e.g. sensory data: text,
; pictures, sound, or perhaps more abstract data.) This could be done
; by loading the analysis Atomese into the same AtomSpace, but this is
; not desirable, as it pollutes the dataset with the analytics code.
;
; The demo below shows how to create a minimally-invasive bootstrap of
; analytics code, loaded from somewhere else, loaded into a child
; AtomSpace of the dataset. The child has full access to the dataset:
; everything is visible to it. But it is distinct: Atoms generated in
; the child space stay in the child space, and don't trash the parent.
;
; The basic assumption is that this is all happening in some
; multi-processing scenario: the contents of the base AtomSpace are
; constantly being updated in one thread, while the analysis is
; happening in another, and so one really wants to keep these separate,
; so that they don't step on one-another.
;
; Notice that (effectively) all code is written in pure Atomese: the
; only "true" scheme code here is the call to `cog-execute!`. The
; reason that everything is in pure Atomese is because that is all that
; can be stored in StorageNodes; one cannot store either scheme or
; python. (Well, technically, with some elbow grease, one could do that,
; but the general intent is not to: the main experiment is to leverage
; the introspection abilities of Atomese; abilities that scheme and
; python lack.
; -----------------------------------------------------------------

(use-modules (opencog) (opencog persist) (opencog persist-rocks))

; Create a child AtomSpace with a unique name (the provided string).
; The `(AtomSpaceOf (Link))` guarantees that the new AtomSpace is a
; layer on top of the current AtomSpace. I guess it does not have to
; be a child, but for now, this seems to be the best way of gaining
; visibility into the parent, without corrupting the parent contents.
(AtomSpace "bootstrap" (AtomSpaceOf (Link)))

; Define a bootstrap sequence. The definition will be in the base space,
; but when executed, the results will be placed in the provided bootstrap
; space. The `PureExecLink` provides this execution isolation.
(cog-execute!
	; Execute a sequence of steps to load the bootstrap space,
	; with contents, and then call a named Atomese snippet to
	; launch executable code.
	(PureExec
		; Where does this all happen? In the child AtomSpace!
		(AtomSpace "bootstrap")

		; Step one: Open the StorageNode:
		(SetValue
			(RocksStorageNode "rocks:///tmp/foo")
			(Predicate "*-open-*")
			(AtomSpace "bootstrap"))

		; Step two: Load up the child space with data:
		(SetValue
			(RocksStorageNode "rocks:///tmp/foo")
			(Predicate "*-load-atomspace-*"))

		; Step three: run something. The assumption here is that
		; the RocksStorageNode contained a PipeLink hooking the
		; NameNode to the rest of the bootup sequence, so that
		; executing it makes the rest of the desired bringup to run.
		; It runs in the bootstrap space, not the data space.
		(Name "bootloader")))

; --------------------------------------------------------------------
; The above, as written, will fail, because the the dataset located
; at /tmp/foo is empty. It is up to you to populate it with something
; meaningful. The code below will do that.

(cog-set-atomspace! (AtomSpace "bootstrap"))

(define (ola) (format #t "hello baby!\n"))
(Pipe
	(Name "bootloader")
	(ExecutionOutput
		(GroundedSchema "scm:ola")
		(List)))

; See that it works:
(cog-execute! (Name "bootloader"))

; Save the entire definition, including the PipeLink. The PipeLink will
; be restored, above during the `*-load-atomspace-*` operation. This
; definition will allow the full bootstrap definition to run. After
; this, you can exit and restart, and verify that everything works;
; be sure, however, to `(define olda ...)` again; this is NOT saved!
(cog-execute!
   (SetValue
      (RocksStorageNode "rocks:///tmp/foo")
      (Predicate "*-store-atom-*")
		(Pipe
			(Name "bootloader")
			(ExecutionOutput
				(GroundedSchema "scm:ola")
				(List)))))

; The End! That's All, Folks!
; --------------------------------------------------------------------
