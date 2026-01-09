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

; This is the end of the line: nothing else makes use of the output of
; this stage. Thus, in order to force the counting to be done, to run
; the whole pipeline, it must be manually executed/triggered.  One word
; of caution: this will run the pipeline each time that it is executed,
; and so will double-count, triple-count, etc. when run more than once.

(cog-execute! (Name "count-types"))
; ---------------------------------------------------------
; ---------------------------------------------------------

; The start of a second data-graphing pipeline. This takes the list of
; types, and de-duplicates it, creating a set in which each Type appears
; only once.  The CollectionOfLink is a container-rewriting tool. It
; accepts as input, any list or set (the (Name "get-types") returns a
; LinkValue) and places that input into a different container: here, a
; UnisetValue, which performs deduplication on its contents.
(Pipe
	(Name "unique-types")
	(CollectionOf (TypeNode 'UnisetValue)
		(Name "get-types")))

; Run the pipeline, if you are curious about what it generates.
; (cog-execute!  (Name "unique-types"))
; ---------------------------------------------------------

; Create a pipeline stage that will sort the set of unique types into
; order by count, with the most frequently-occuring types first. This
; requires creating a comparison relation that will determine the sort
; order.
;
; The order relation is presumably obvious: more or less.
; * It takes two inputs, left and right.
; * It gets the Value attached at the key (Preicate "cnt") on each.
; * This value was a vector, of the form (0 0 N) for some count N..
; * The ElementOf extracts the third number, zero-based.
; * The LessThan compares the left and right counts.
; * The NotLink is a trick, to make suere that N <> N, so thtat two
;   different types, having the same count, compare as not-equal.
;   Were they treated as equal, then one of the two would have been
;   discarded.
; * The DefineLink is used, instead of PipeLink, because this is a
;   function defintion, not a pipeline stage. The function has two
;   free-floationg inputs: left and right, that are not attached to
;   anything. The VariableNodes assign names to the inputs, and are
;   used in the "internal wiring" of the function. The Define is used
;   to give a name the function.

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

; A pipeline stage that converts the input set into a sorted set.
; Previously, that conversion was done with a `CollectionOfLink`;
; here, the `LinkSignatureLink` is used. The difference between the
; two hinges on a technicality.
(Pipe
	(Name "sorted-types")
	(LinkSignature
		(TypeNode 'SortedValue)
		(DefinedPredicate "count-order")
		(Name "unique-types")))

; ---------------------------------------------------------
; Print (output) the counts in a format such that some graphing
; system can graph them.
;
; Interfaces to the external world; the world outside of Atomese,
; presents practical difficulties. We will jump through some hoops,
; to demo. For starters, do it the "olde-fashioned" way: use a short
; snippet of scheme code to print the counts into a string.

(define (data-printer NAME COUNT)
	(StringValue
		(format #f "Usage count of type: ~A ~A"
			(cog-value-ref COUNT 2)
			(cog-name NAME))))

; Define a simple but inadequate printer. It should be easy to
; understand how this printer works, which is why it is presented
; in this demo. However: it fails, as not all TypeNodes will have
; a count on them, an an exception will be thrown, when attempting
; to access the count. The reason that not everything has a count
; yet is because we ran `(cog-execute! (Name "count-types"))` a long
; long time ago, and added many new types to the AtomSpace since then.
; We could fix this by running the counter again. But there's also
; another way.
(define simple-printer
	(Filter
		(Rule
			(TypedVariable (Variable "$typ") (Type 'Type)) ; vardecl
			(Variable "$typ") ; body - accept everything
			(ExecutionOutput
				(GroundedSchema "scm:data-printer")
				(LinkSignature (Type 'LinkValue)
					(Variable "$typ")
					(ValueOf (Variable "$typ") (Predicate "cnt")))))
		(Name "sorted-types")))

; Invoke the printer above. Watch it throw.
; (cog-execute! simple-printer)

; Define a fancier printer that guards against absent counts.
; This uses a predicate -- the EqualLink, to validate the
; expected signature.
(define guarded-printer
	(Filter
		(Rule
			(TypedVariable (Variable "$typ") (Type 'Type)) ; vardecl
			(And
				(Variable "$typ") ; body - accept everything
				(Equal            ; evaluatable guard
					(Type 'FloatValue)
					(TypeOf (ValueOf (Variable "$typ") (Predicate "cnt")))))

			(ExecutionOutput
				(GroundedSchema "scm:data-printer")
				(LinkSignature (Type 'LinkValue)
					(Variable "$typ")
					(ValueOf (Variable "$typ") (Predicate "cnt")))))
		(Name "sorted-types")))

; This one works fine.
(cog-execute! guarded-printer)

; -------------------------------------------------------------
; A fancier way to arrive at the above is to do the string
; manipulations entirely in Atomese. This is shown below.

(Pipe
	(Name "list of structures")
	(Filter
		(Rule
			(TypedVariable (Variable "$typ") (Type 'Type)) ; vardecl
			; Specify a body that rejects types without counts.
			(And
				(Present (Variable "$typ"))
				(Equal
					(Type 'FloatValue)
					(TypeOf (ValueOf (Variable "$typ") (Predicate "cnt")))))
			; Package up the results into a "structure" consisting
			; Values of various types -- some strings interleaved
			; with some numbers.
			(LinkSignature (Type 'LinkValue)
				(Node "Usage count of type: ")
				(Variable "$typ")
				(Node " is equal to ")
				(ElementOf (Number 2)
					(ValueOf (Variable "$typ") (Predicate "cnt")))
				(Node "\n")))
		(Name "sorted-types")))

; Run this pipeline.
(cog-execute! (Name "list of structures"))

; -------------------------------------------------------------
; The above needs to be converted to pure strings. Below is a very
; ugly way to do this conversion.  It ... works. Something better
; would be better.

(Pipe
	(Name "string repr")
	(Filter
		(Rule
			(VariableList
				(Variable "$a")
				(Variable "$b")
				(Variable "$c")
				(Variable "$d")
				(Variable "$e"))
			(LinkSignature (Type 'LinkValue)
				(Variable "$a")
				(Variable "$b")
				(Variable "$c")
				(Variable "$d")
				(Variable "$e"))
			(TransposeColumn
				(LinkSignature (Type 'LinkValue)
					(LinkSignature (Type 'StringValue) (Variable "$a"))
					(LinkSignature (Type 'StringValue) (Variable "$b"))
					(LinkSignature (Type 'StringValue) (Variable "$c"))
					(LinkSignature (Type 'StringValue) (Variable "$d"))
					(LinkSignature (Type 'StringValue) (Variable "$e")))))
		(Name "list of structures")))

(cog-execute! (Name "string repr"))

; Flatten the structure above
(cog-execute! (Concatenate (Name "string repr")))

; -------------------------------------------------------------
; Use the TextFileNode from the sensory subsystem to write the above
; out to a file. This part of the demo requires the sensory subsystem
; to be installed.

(use-modules (opencog sensory))

(define outfile (TextFile "file:///tmp/count-pipeline.txt"))

(cog-execute!
	(SetValue outfile (Predicate "*-open-*") (Type 'StringValue)))

(cog-execute!
	(SetValue outfile (Predicate "*-write-*") (Name "string repr")))

(cog-execute!
	(SetValue outfile (Predicate "*-close-*") (Node "")))

; Take a look: cat /tmp/count-pipeline.txt
;
; The End! That's All, Folks!
; -------------------------------------------------------------
