;
; episodic-space.scm -- Demo of storing AtomSpaces within Atoms
;
; There are many ways to represent episodic memories; one particularly
; intersting one is to store them into AtomSpaces that are then attached
; as a Value on an Atom. This offers several benefits over other
; representations: the entire collection can be treated as a coherent
; whole, and thus added or removed at will. Atoms located in such spaces
; are not in the mainspace, and thus do not pollute the mainspace,
; avoiding cross-talk and naming collisions. Searches can be limited to
; the episodic subspace. Different episodic subspaces can be joined
; together, using the frame concept. Thus, storing AtomSpaces as values
; provides multiple advantages over using one giant AtomSpace for
; everything. This demo example shows how this can be done.
;
; ---------------------------------------------------------------------
(use-modules (opencog))

; Get a reference to the current AtomSpace; this is our main space.
(define base-space (cog-atomspace))

; Add some Atom. Anything will do.
(ConceptNode "foo")

; Breif review of how Values work. Values can be vectors of numbers,
; srings, or even of other Atoms. In just a moment, we'll also be
; attaching AtomSpaces, as well.
(cog-set-value!
	(ConceptNode "foo") (Predicate "bunch o numbers")
		(FloatValue 1 2 3.14159 4 5 6))

(cog-set-value!
	(ConceptNode "foo") (Predicate "some words")
		(StringValue "once" "upon" "a" "time"))

(cog-set-value!
	(ConceptNode "foo") (Predicate "some atoms")
		(LinkValue (Concept "dog") (Concept "cat") (Concept "mouse")))

; Verify that the above worked. `cog-keys` returns all the keys
; attached to the Atom, and `cog-value` returns the value for a
; specific key.
(cog-keys (ConceptNode "foo"))
(cog-value (ConceptNode "foo") (Predicate "some words"))

; Print a summary report of what kinds of Atoms ar in the AtomSpace.
(cog-report-counts)

; And now, the main event: create some AtomSpaces, and store them as
; values, too. AtomSpaces will be identified by name. Thus, they behave
; much like Nodes. Some advice about naming and name collisions will be
; given later. So, just like above, put some AtomSpaces in the key-value
; store:
(cog-set-value!
	(ConceptNode "foo") (Predicate "real life")
		(AtomSpace "happy thoughts"))

(cog-set-value!
	(ConceptNode "foo") (Predicate "repressed mem")
		(AtomSpace "crushing defeat"))

; As before, verify that the AtomSpaces can be found at the given keys.
(cog-value (ConceptNode "foo") (Predicate "real life"))
(cog-value (ConceptNode "foo") (Predicate "repressed mem"))

; To continue with the demo, populate these AtomSpaces with some data.
; Since the guile bindings always work with a current atomspace (per
; thread), we have to swtich to that to use it.
(define as-one (cog-value (ConceptNode "foo") (Predicate "real life")))
(cog-set-atomspace! as-one)

; Add some content.
(ListLink (Concept "mom") (Concept "dad"))
(ListLink (Concept "first crush") (Concept "Gilanda"))

; Review the content.
(cog-prt-atomspace)

; Switch to the other AtomSpace.
(cog-set-atomspace! base-space)
(cog-set-atomspace!
	(cog-value (ConceptNode "foo") (Predicate "repressed mem")))

; Add some content, and review it.
(ListLink (Concept "misdemeanor") (Concept "vandalism"))
(ListLink (Concept "furious") (Concept "anger"))
(cog-prt-atomspace)

; Return to the base AtomSpace, and make sure it does NOT contain
; anything that was added to the other two. The look at the other two,
; to make sure they haven't changed, and still contain what they did
; before.
(cog-set-atomspace! base-space)
(cog-prt-atomspace)
(cog-prt-atomspace (cog-value (ConceptNode "foo") (Predicate "real life")))
(cog-prt-atomspace (cog-value (ConceptNode "foo") (Predicate "repressed mem")))

; Some commentary about AtomSpace management (this is important!) The
; `AtomSpace` function is defined as
;      (define-public (AtomSpace x)
;          (cog-add-atomspace (cog-new-atomspace x)))
; This it creates a new AtomSpace *and* inserts it into the current
; AtomSpace. The name is used to identify it, and so it behave a lot
; like a Node. For example, saying `(AtomSpace "bar")` twice in a row
; will return the *same* AtomSpace. Thus (just like a Node) it is enough
; to know it's name.
;
; By contrast, `(cog-new-atomspace x)` create a new AtomSpace every time
; it is called, so calling `(cog-new-atomspace "blah")` twice in a row
; will give you two *different* AtomSpaces, having the same name. Both
; of them will be floating in the void, and so if you drop references to
; them, they will be garbage-collected. To get one unique copy, you must
; use `cog-add-atomspace`, with will return the oldest AtomSpace having
; the given name.
;
; ------------------------------------------------------
; All of the above is very interesting, but useless if we can't save
; and restore contents. So the rest of this demo examines how to do that.

; As usual, load the modules that provide storage.
(use-modules (opencog persist))
(use-modules (opencog persist-file))

; We'll store to a file in the temp directory.
; Open it, save, and close
(define fsn (FileStorageNode "/tmp/foo"))
(cog-open fsn)
(store-atomspace)
(cog-close fsn)

; Now, edit `/tmp/foo` with your favorite file editor. Notice that
; it contains the contents of the main space, but NOT of the episodic
; spaces. These need to be saved explicitly, and in seperate files.
; Why? The whole point of episodic memory is that you don't have to
; deal with one giant ball with everything in it; instead, you can work
; with only those chunks that you want and need, when you want them,
; and as you need them.

; So, let's save the other two parts.
(define as-one (cog-value (ConceptNode "foo") (Predicate "real life")))
(define as-two (cog-value (ConceptNode "foo") (Predicate "repressed mem")))
(define fsa (FileStorageNode "/tmp/foo-one"))
(define fsb (FileStorageNode "/tmp/foo-two"))
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

; View `/tmp/foo-one` and `/tmp/foo-two` and verify that they contain
; what is expected.

; ------------------------------------------------------
; For the next part of the demo, you should exit the guile shell,
; and restart it from scratch. The goal here is to restore the contents
; from the saved files.
;
; The code below is nearly identical to the store code above, except
; that it loads, instead of storing. Otherise, its just a cut-n-paste.

(use-modules (opencog) (opencog persist))
(use-modules (opencog persist-file))

; Save a reference to our main working space.
(define base-space (cog-atomspace))

(define fsn (FileStorageNode "/tmp/foo"))

(cog-open fsn)
(load-atomspace)
(cog-close fsn)

; Verify that the contents are as expected.
(cog-prt-atomspace)
(cog-keys (Concept "foo"))
(cog-value (Concept "foo") (Predicate "real life"))

; Now, restore the two batches of episodic memories.
(define as-one (cog-value (ConceptNode "foo") (Predicate "real life")))
(define as-two (cog-value (ConceptNode "foo") (Predicate "repressed mem")))
(define fsa (FileStorageNode "/tmp/foo-one"))
(define fsb (FileStorageNode "/tmp/foo-two"))
(cog-open fsa)
(cog-open fsb)
(cog-set-atomspace! as-one)
(load-atomspace fsa)
(cog-close fsa)
(cog-set-atomspace! as-two)
(load-atomspace fsb)
(cog-close fsb)

; Return to home base.
(cog-set-atomspace! base-space)

; Verify that the contents are as expected
(cog-prt-atomspace)
(cog-prt-atomspace as-one)
(cog-prt-atomspace as-two)

; ------------------------------------------------------
; Much as above, except that RocksDB is used, instead of a flat file.
(use-modules (opencog persist))
(use-modules (opencog persist-rocks))

(define rsn (RocksStorageNode "rocks:///tmp/blob"))

(cog-open rsn)
(store-atomspace)
(cog-close rsn)

(define as-one (cog-value (ConceptNode "foo") (Predicate "real life")))
(define as-two (cog-value (ConceptNode "foo") (Predicate "repressed mem")))

(cog-open rsn)
(store-atomspace (cog-value (ConceptNode "foo") (Predicate "real life")))
(store-atomspace (cog-value (ConceptNode "foo") (Predicate "repressed mem")))
(cog-close rsn)

; ------------------------------------------------------

(use-modules (opencog) (opencog persist))
(use-modules (opencog persist-rocks))

(define base-space (cog-atomspace))
(define rsn (RocksStorageNode "rocks:///tmp/blob"))

(cog-open rsn)
(load-frames)
(load-atomspace)
(cog-close rsn)
(cog-prt-atomspace)

(cog-prt-atomspace (cog-value (ConceptNode "foo") (Predicate "real life")))
(cog-prt-atomspace (cog-value (ConceptNode "foo") (Predicate "repressed mem")))
