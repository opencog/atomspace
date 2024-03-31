;
; file-episodic.scm -- Unit test for FileStorageNode w/AtomSpaces
;
; This is a modified copy of the `episodic-space.scm` demo in the main
; examples directory.
;
(use-modules (opencog) (opencog persist) (opencog persist-file))
(use-modules (opencog test-runner))

; ---------------------------------------------------------------------
; Create a unique file name.
(set! *random-state* (random-state-from-platform))
(define fbase (format #f "/tmp/opencog-test-~D.scm" (random 1000000000)))
(define fsub1 (format #f "/tmp/opencog-test-~D.scm" (random 1000000000)))
(define fsub2 (format #f "/tmp/opencog-test-~D.scm" (random 1000000000)))

(format #t "Using files:\n~A\n~A\n~A\n" fbase fsub1 fsub2)

; ---------------------------------------------------------------------
(opencog-test-runner)
(define tname "store_episodes")
(test-begin tname)

; Get a reference to the current AtomSpace; this is our main space.
(define base-space (cog-atomspace))
(ConceptNode "foo")
(cog-set-value! (ConceptNode "foo") (Predicate "bunch o numbers")
		(FloatValue 1 2 3.14159 4 5 6))
(cog-set-value! (ConceptNode "foo") (Predicate "some words")
		(StringValue "once" "upon" "a" "time"))
(cog-set-value! (ConceptNode "foo") (Predicate "some atoms")
		(LinkValue (Concept "dog") (Concept "cat") (Concept "mouse")))

(cog-set-value! (ConceptNode "foo") (Predicate "real life")
		(AtomSpace "happy thoughts"))

(cog-set-value! (ConceptNode "foo") (Predicate "repressed mem")
		(AtomSpace "crushing defeat"))

; Populate the subspaces
(cog-set-atomspace!
	(cog-value (ConceptNode "foo") (Predicate "real life")))

; Add some content.
(ListLink (Concept "mom") (Concept "dad"))
(ListLink (Concept "first crush") (Concept "Gilanda"))
(ListLink (Concept "stack blocks"))

; Switch to the other AtomSpace.
(cog-set-atomspace! base-space)
(cog-set-atomspace!
	(cog-value (ConceptNode "foo") (Predicate "repressed mem")))

(ListLink (Concept "misdemeanor") (Concept "vandalism"))
(ListLink (Concept "furious") (Concept "anger"))

; Return to the main space.
(cog-set-atomspace! base-space)

(format #t "Actual counts are ~A ~A ~A\n"
	(count-all)
	(count-all (cog-value (ConceptNode "foo") (Predicate "real life")))
	(count-all (cog-value (ConceptNode "foo") (Predicate "repressed mem"))))

; Verify contents
(test-assert "base-count" (equal? 11 (count-all)))
(test-assert "space1-count" (equal? 8
	(count-all (cog-value (ConceptNode "foo") (Predicate "real life")))))
(test-assert "space2-count" (equal? 6
	(count-all (cog-value (ConceptNode "foo") (Predicate "repressed mem")))))

; Dump to files
(define fsn (FileStorageNode fbase))
(cog-open fsn)
(store-atomspace)
(cog-close fsn)

; So, let's save the other two parts.
(define as-one (cog-value (ConceptNode "foo") (Predicate "real life")))
(define as-two (cog-value (ConceptNode "foo") (Predicate "repressed mem")))
(define fsa (FileStorageNode fsub1))
(define fsb (FileStorageNode fsub2))
(cog-open fsa)
(cog-open fsb)
(cog-set-atomspace! as-one)
(store-atomspace fsa)
(cog-close fsa)
(cog-set-atomspace! as-two)
(store-atomspace fsb)
(cog-close fsb)

; Return to home base.
(cog-set-atomspace! base-space)

; Verify that the contents are as expected
(cog-prt-atomspace)
(cog-prt-atomspace as-one)
(cog-prt-atomspace as-two)

; clober temps
(set! as-one #f)
(set! as-two #f)
(set! fsa #f)
(set! fsb #f)
(set! fsn #f)
(gc) (gc)

(cog-atomspace-clear)

; ---------------------------------------------------------------------

; Load everything from the file.

(define gsn (FileStorageNode fbase))
(define gsa (FileStorageNode fsub1))
(define gsb (FileStorageNode fsub2))

(cog-open gsn)
(load-atomspace)
(cog-close gsn)

(test-assert "base-count" (equal? 11 (count-all)))
(test-assert "space1-count" (equal? 0
	(count-all (cog-value (ConceptNode "foo") (Predicate "real life")))))
(test-assert "space2-count" (equal? 0
	(count-all (cog-value (ConceptNode "foo") (Predicate "repressed mem")))))

; Now, restore the two batches of episodic memories.
(define as-one (cog-value (ConceptNode "foo") (Predicate "real life")))
(define as-two (cog-value (ConceptNode "foo") (Predicate "repressed mem")))
(cog-open gsa)
(cog-open gsb)
(cog-set-atomspace! as-one)
(load-atomspace gsa)
(cog-close gsa)
(cog-set-atomspace! as-two)
(load-atomspace gsb)
(cog-close gsb)

; Return to home base.
(cog-set-atomspace! base-space)

(test-assert "base-count" (equal? 11 (count-all)))
(test-assert "space1-count" (equal? 8
	(count-all (cog-value (ConceptNode "foo") (Predicate "real life")))))
(test-assert "space2-count" (equal? 6
	(count-all (cog-value (ConceptNode "foo") (Predicate "repressed mem")))))

; Verify that the contents are as expected
(cog-prt-atomspace)
(cog-prt-atomspace as-one)
(cog-prt-atomspace as-two)
; --------------------------
; Clean up.
(delete-file fbase)
(delete-file fsub1)
(delete-file fsub2)

(test-end tname)

(opencog-test-end)
