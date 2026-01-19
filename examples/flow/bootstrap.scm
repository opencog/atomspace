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
; -----------------------------------------------------------------

(use-modules (opencog) (opencog persist) (opencog persist-rocks))

; Create a child AtomSpace with a unique name (the provided string).
; The `(AtomSpaceOf (Link))` guarantees that the new AtomSpace is a
; layer on top of the current AtomSpace. I guess it does not have to
; be a child, but for now, this seems to be the best way of gaining
; visibility into the parent, without corrupting the parent contents.
(AtomSpace "bootstrap" (AtomSpaceOf (Link)))

; Define a bootstrap sequence. The definition will be in the base space,
; but when executed, the results will be placed in the provided
; bootstrap space. The `PureExecLink` provides this execution isolation.
(cog-execute!
	; Execute a sequence of steps to load the bootstrap space,
	; and get things started in there.
	(PureExec
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
		(Name "bootloader")

		; Where does this all happen? In the child AtomSpace!
		(AtomSpace "bootstrap")))
