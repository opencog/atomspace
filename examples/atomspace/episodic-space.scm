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
		(ListValue (Concept "dog") (Concept "cat") (Concept "mouse")))

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

; Now for some (very important!) commentary about AtomSpace naming and
; AtomSpace uniqueness, and AtomSpace collisions. You'll get a mess, if
; you don't understand this clearly. So, first of all, every time you
; say `(AtomSpace "blah")` in scheme, a brand-new AtomSpace gets created.
; If you say `(AtomSpace "blah")` twice, you will get *two* different
; AtomSpaces. This is not how Nodes ordinarily behave: if you were to say
; ... Hmmm Lets change this.
; to you to avoid naming collisions
; ------------------------------------------------------

(use-modules (opencog persist))
(use-modules (opencog persist-file))

(define fsn (FileStorageNode "/tmp/foo"))

(cog-open fsn)
(store-atomspace)
(cog-close fsn)

(use-modules (opencog persist-rocks))

(define rsn (RocksStorageNode "rocks://tmp/blob"))

(cog-open rsn)
(store-atomspace)
(cog-close rsn)

; ------------------------------------------------------

(use-modules (opencog) (opencog persist))
(use-modules (opencog persist-rocks))

(define rsn (RocksStorageNode "rocks://tmp/blob"))

(cog-open rsn)
(load-atomspace)
(cog-close rsn)
(cog-prt-atomspace)

; ------------------------------------------------------
(use-modules (opencog) (opencog persist))
(use-modules (opencog persist-file))

(define fsn (FileStorageNode "/tmp/foo"))

(cog-open fsn)
(load-atomspace)
(cog-close fsn)
(cog-prt-atomspace)
