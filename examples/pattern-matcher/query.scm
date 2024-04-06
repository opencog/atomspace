;
; query.scm -- Parallel queries (parallel pattern matching)
;
; QueryLink usage example.
;
; The QueryLink and the BindLink are both very similar; both search
; the AtomSpace for groundings of the query pattern, and then perform
; a re-write, based on the results. The only difference between the two
; is that the BindLink returns a SetLink containing the results, whereas
; the QueryLink returns a LinkValue containing the results. This makes
; the QueryLink a bit nicer, because it does not pollute the AtomSpace
; with nearly-useless SetLinks.
;
; Although both can be run in parallel (i.e. run in different threads),
; there's almost no point to doing so for the BindLink, since you have
; to wait for it to complete, and provide you with the resulting
; SetLink. By contrast, the QueryLink can drop off results at a
; "well-known location" in the AtomSpace, as they appear, so that
; processing can happen in parallel: processing can start on some
; results, even while others are still being found.
;
; This example uses an AnchorNode to establish a "well-known location",
; a QueryLink to attach them there, and a DeleteLink to detach results
; from the AnchorNode. The  ParallelLink is used to run multiple threads,
; and SleepLink to slow it down enough to see what is happening.
;
; Taken as a whole, it demos a toy parallel processing pipeline.
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

			; Immediately detach from that anchor, by deleting the
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
	(format #t "I think that ~A is ~A. -- ~A\n"
		(cog-name NODE-A) (cog-name NODE-B)
		(strftime "%c" (localtime (current-time)))
	)
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

			; Immediately detach from that anchor, by deleting the
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

; Double-check that inputs have been consumed, by looking at the anchor
; point.
(cog-incoming-set (AnchorNode "*-risible results-*"))

; -------------
; Now, assemble an automated processing pipeline.

; Print the current time
(define (prti N)
	(format #t "Thread ~A. -- ~A\n" (cog-name N)
		(strftime "%c" (localtime (current-time))))
	(SimpleTruthValue 1 1))

; Atom to print the current time
(define (prtime STR)
	(Evaluation
		(GroundedPredicate "scm:prti")
		(Concept STR)))

; When executed, this launches three threads, and returns to the
; caller without any delay.  The threads run to conclusion, and
; then quietly exit.
(define threads
	(Parallel
		(SequentialAnd
			(prtime "step one A")
			(True query)
			(True (Sleep (Number 4)))
			(prtime "step one B")
			(True query)
			(True (Sleep (Number 4)))
			(prtime "step one C")
			(True query))

		(SequentialAnd
			(True (Sleep (Number 1)))
			(prtime "step two A")
			(True absurd)
			(True (Sleep (Number 4)))
			(prtime "step two B")
			(True absurd)
			(True (Sleep (Number 4)))
			(prtime "step two C")
			(True absurd))

		(SequentialAnd
			(True (Sleep (Number 2)))
			(prtime "step three A")
			(True output)
			(True (Sleep (Number 4)))
			(prtime "step three B")
			(True output)
			(True (Sleep (Number 4)))
			(prtime "step three C")
			(True output))
	))

; Run the multi-threaded pipeline. This should print some fairly verbose
; messages, which hopefully makes clear what is going on.
;
; (cog-execute! threads)
