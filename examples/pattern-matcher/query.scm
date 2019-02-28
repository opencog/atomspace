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
; the ParallelLink to run multiple threads, and a DeleteLink to dettach
; results from the AnchorLink. The result is a toy parallel processing
; pipeline.
;

(use-modules (opencog) (opencog exec))

; -------------
; Create three bits of "knowledge".
(Evaluation
	(Predicate "foobar") (List (Concept "funny") (Concept "thing")))
(Evaluation
	(Predicate "foobar") (List (Concept "funny") (Concept "story")))
(Evaluation
	(Predicate "foobar") (List (Concept "funny") (Concept "joke")))

; -------------
; Define a simple query. It looks for the funny stuff, and attaches
; the result to an AnchorNode
(define query
	(Query
		(TypedVariable (Variable "$x") (Type 'ConceptNode))
		(Evaluation
			(Predicate "foobar")
			(List (Concept "funny") (Variable "$x")))
		(ListLink
			(Anchor "*-query results-*")
			(Implication (Variable "$x") (Concept "laughable")))
	))

; Actually run it - this should return all of the results, wrapped
; in a LinkValue.
(cog-execute! query)

; Take a look at the incoming set of the anchor point, and verify
; that the expected content is there.
(cog-incoming-set (Anchor "*-query results-*"))

; -------------
; Define a second stage to the processing pipeline
(define absurd
	(Query
		(TypedVariable (Variable "$x") (Type 'ConceptNode))
		(And
			; Search at the earlier anchor point.
			(Present (ListLink
				(Anchor "*-query results-*")
				(Implication (Variable "$x") (Concept "laughable"))))

			; Immediately dettach from that anchor, by deleting the
			; ListLink that couples the two together.
			(True (Delete (ListLink
				(Anchor "*-query results-*")
				(Implication (Variable "$x") (Concept "laughable"))))))

		; After matching the above, create an attachment to the
		; second stage anchor point.
		(ListLink
			(Anchor "*-risible results-*")
			(Implication (Variable "$x") (Concept "ludicrous")))
	))

; Run the query. See what happens.
(cog-execute! absurd)

; Verify that the old anchor point has been vacated, as expected.
(cog-incoming-set (Anchor "*-query results-*"))

; Verify that the results are now at the new anchor
(cog-incoming-set (Anchor "*-risible results-*"))

; -------------
; Now define a third stage of processing. This will generate output,
; i.e. it will print something to stdout.
;
(define (report-stuff NODE-A NODE-B)
	(format #t "I think that ~A is ~A\n"
		(cog-name NODE-A) (cog-name NODE-B))
	(SimpleTruthValue 1 1))

(define output
	(Query
		(VariableList
			(TypedVariable (Variable "$x") (Type 'ConceptNode))
			(TypedVariable (Variable "$y") (Type 'ConceptNode)))
		(And
			; Search at the earlier anchor point.
			(Present (ListLink
				(Anchor "*-risible results-*")
				(Implication (Variable "$x") (Variable "$y"))))

			; Immediately dettach from that anchor, by deleting the
			; ListLink that couples the two together.
			(True (Delete (ListLink
				(Anchor "*-risible results-*")
				(Implication (Variable "$x") (Variable "$y"))))))

		; After matching the above, print a report.
		(ExecutionOutput
			(GroundedSchema "scm:report-stuff")
			(ListLink (Variable "$x") (Variable "$y")))
	))

; Run it. Verify that it works.
(cog-execute! output)

; Run it a second time, verify that all inputs have been consumed.
(cog-execute! output)

; Double-check that inputs have been consumed, by looking at the atnchor
; point.
(cog-incoming-set (AnchorNode "*-risible results-*"))

; -------------
; Now, assemble an automated processing pipeline.
