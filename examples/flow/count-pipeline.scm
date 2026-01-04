;
; count-pipeline.scm -- multi-stage pipeline that counts things
;
; Named pipes allow long data procesing systems to be constructed.
; This demo creates a multi-stage pipeline that counts how often
; different Atom types appear in the AtomSpace, sorting the couts,
; and finally printing the results in a form suitale for graphing.
;
; The pipeline processing stages are written in "pure" Atomese,
; avoiding scheme as much as (currently) possible.  Each stage of the
; pipeline is a small individual step. Many of these stages could have
; been combined to create a shorter pipeline which larger more complex
; steps. It seems bette, however, to leave that as an exercise to the
; reader, so as to make the demo as easy to understand as possible.
;
(use-modules (opencog))

; The start of the pipeline. Define a MeetLink that, when executed,
; will return a list of all Atoms in the AtomSpace. Executing the
; NameNode will cause the MeetLink to run. That is, the NameNode
; assigns a name to the output of the MeetLink.
(PipeLink
	(Name "get-all-atoms")
	(Meet
		(Variable "$atom") ; vardecl
		(Variable "$atom") ; match anything, everything
	))

; Execute, if desired, to visually verify that the expected results
; can be obtained. Note: the results are NOT cached: each execution
; will return the AtomSpace contents, at the time of execution.
; (cog-execute! (Name "get-all-atoms"))
; ---------------------------------------------------------

; Define a rewrite rule that will convert the list of all Atoms to a
; list of the Type of each Atom. This rule is chained to the end of the
; getter, above, so that when this rule is run, it will run the earlier
; step in the chain (to get it's input data.)
;
; The rule itself is presumably self-explanatory: it accepts any Atom,
; and then applies TypeOfLink to it.
(PipeLink
	(Name "get-types")
	(Filter
		(Rule
			(TypedVariable (Variable "$atom") (Type 'Atom)) ; vardecl
			(Variable "$atom") ; body - accept everything
			(TypeOf (DontExec (Variable "$atom")))) ; rewrite
		(Name "get-all-atoms")))

; Execute, if desired, to visually verify the results. Executing this
; will run the full pipeline: A fresh copy of the AtomSpace contents
; will be fetched before applying the TypeOf rewrite rule.
; (cog-execute! (Name "get-types"))
; ---------------------------------------------------------

; Chain a rewrite that counts each Type as it occurs. As before, this
; is chained onto the end of the previous pipe. The variable declaration
; for the rewrite rule accepts only TypeNodes; this is not really needed
; here but is illuustrative. The count attaches a vector to the
; TyppeNode itself, and increments this by the vector "0 0 1". Why this?
; No particular reason; it serves only to illustrate that the increments
; are themselves vectors (of arbitary length).
;
(Pipe
	(Name "count-types")
	(Filter
		(Rule
			(TypedVariable (Variable "$typ") (Type 'Type)) ; vardecl
			(Variable "$typ") ; body - accept everything
			(IncrementValue (Variable "$typ") (Predicate "cnt") (Number 0 0 1)))
		(Name "get-types")))

; This is the end of the line: nothing else makes use of the output
; of this stage.
; The above
; Must actually run this one...
; Can we have an Atom that just runs where created?
(cog-execute! (Name "count-types"))
; ---------------------------------------------------------

(Pipe
	(Name "unique-types")
	(CollectionOf (TypeNode 'UnisetValue)
		(Name "get-types")))

; (cog-execute!  (Name "unique-types"))

(DefineLink
	(DefinedPredicate "count-order")
	(Lambda
		(VariableList (Variable "left") (Variable "right"))
		(Not
			(LessThan
				(ElementOf (Number 2)
					(ValueOf (Variable "left") (Predicate "cnt")))
				(ElementOf (Number 2)
					(ValueOf (Variable "right") (Predicate "cnt")))))))

(Pipe
	(Name "sorted-types")
	(LinkSignature
		(TypeNode 'SortedValue)
		(DefinedPredicate "count-order")
		(Name "unique-types")))


; Debug print
(cog-execute!
	(Filter
		(Rule
			(TypedVariable (Variable "$typ") (Type 'Type)) ; vardecl
			(Variable "$typ") ; body - accept everything
			(LinkSignature (Type 'LinkValue)
				(Variable "$typ")
				(ValueOf (Variable "$typ") (Predicate "cnt"))))
		(Name "sorted-types")))

