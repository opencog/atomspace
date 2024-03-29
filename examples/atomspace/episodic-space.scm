

(use-modules (opencog))

(define base-space (cog-atomspace))
(ConceptNode "foo")

(cog-set-value!
	(ConceptNode "foo") (Predicate "bunch o numbers")
		(FloatValue 1 2 3.14159 4 5 6))

(cog-set-value!
	(ConceptNode "foo") (Predicate "some words")
		(StringValue "once" "upon" "a" "time"))

(cog-keys (ConceptNode "foo"))
(cog-value (ConceptNode "foo") (Predicate "some words"))

(cog-report-counts)


(cog-set-value!
	(ConceptNode "foo") (Predicate "real life")
		(cog-new-atomspace "happy thoughts"))

(cog-set-value!
	(ConceptNode "foo") (Predicate "repressed mem")
		(cog-new-atomspace "crushing defeat"))

(cog-value (ConceptNode "foo") (Predicate "real life"))
(cog-value (ConceptNode "foo") (Predicate "repressed mem"))

(cog-set-atomspace!
	(cog-value (ConceptNode "foo") (Predicate "real life"))
(ListLink (Concept "mom") (Concept "dad"))
(ListLink (Concept "first crush") (Concept "Gilanda"))
(cog-prt-atomspace)

(cog-set-atomspace!
	(cog-value (ConceptNode "foo") (Predicate "repressed mem")))

(ListLink (Concept "misdemeanor") (Concept "vandalism"))
(ListLink (Concept "furious") (Concept "anger"))
(cog-prt-atomspace)

(cog-set-atomspace! base-space)
(cog-prt-atomspace)
(cog-prt-atomspace (cog-value (ConceptNode "foo") (Predicate "real life")))
(cog-prt-atomspace (cog-value (ConceptNode "foo") (Predicate "repressed mem")))


(use-modules (opencog persist))
(use-modules (opencog persist-file))

(define fsn (FileStorageNode "file:/tmp/foo"))

(cog-open fsn)
(store-atomspace)
(cog-close fsn)
(use-modules (opencog persist-rocks))

(define rsn (RocksStorageNode "rocks://tmp/blob"))

(cog-open rsn)
(store-atomspace)
(cog-close rsn)


(use-modules (opencog) (opencog persist))
(use-modules (opencog persist-rocks))

(define rsn (RocksStorageNode "rocks://tmp/blob"))

(cog-open rsn)
(load-atomspace)
(cog-close rsn)
(cog-prt-atomspace)

