;
; query.scm -- Parallel queries (parallel pattern matching)
;
; QueryLink usage example.
;
; The QueryLink and the BindLink are both very similar; both search
; the atomspace for groundings of the query pattern, and then perform
; a re-write, based on the results. The only difference between the two
; is that the BindLink returns a SetLink containing the results, whereas
; the QueryLink returns a LinkValue containing the results. This makes
; the QueryLink a bit nicer, because it does not pollute the AtomSpace
; with nearly-useless SetLinks.
;
; Although both can be run in parallel (i.e. run in different threads),
; there's almost no point to doing so for the BindLink, since you have
; to wait for it to complete, and drop the result SetLink on your
; doorstep. By contrast, they QueryLink can drop off results at a
; "well-known location" in the atomspace, as they appear, so that
; processing can happen in parallel: processing can start on some
; results, even while others are still being found.
;
; This example uses an AchorNode to establish a "well-known location",
; the ParallelLink to run multiple threads, and other trickery to create
; a parallel processing pipeline.
;

(use-modules (opencog) (opencog exec))

; Create three bits of "knowledge".
(Evaluation
	(Predicate "foobar") (List (Concept "funny") (Concept "thing")))
(Evaluation
	(Predicate "foobar") (List (Concept "funny") (Concept "story")))
(Evaluation
	(Predicate "foobar") (List (Concept "funny") (Concept "joke")))

; Define a simple query. It looks for the funny stuff, and attaches
; the result to an AnchorNode
(define query
	(Query
		(TypedVariable (Variable "$x") (Type 'ConceptNode))
		(Evaluation
			(Predicate "foobar")
			(List (Concept "funny") (Variable "$x")))
		(ListLink
			(Anchor "*-query reults -*")
			(Implication (Variable "$x") (Concept "laughable")))
	))

; Actually run it - this should return all of the results, wrapped
; in a LinkValue.
(cog-execute! query)
