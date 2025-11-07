
(use-modules (opencog) (opencog exec))

; A simple DAG w/ InheritanceLinks
;
;     A
;     |
;     B
;    / \
;   C   F
;   |
;   D
;
(Inheritance    (Concept "A")    (Concept "B"))
(Inheritance    (Concept "B")    (Concept "C"))
(Inheritance    (Concept "B")    (Concept "F"))
(Inheritance    (Concept "C")    (Concept "D"))

; --------------------------------------------------------------

; The re-written DAG edges will look like this:
(define into-form
	(Edge (Predicate "yikes")
		(ListLink (Variable "$head") (Variable "$tail"))))

; A defined Lambda, in atomese.
(DefineLink
	(DefinedSchemaNode "make-an-edge")
	(Lambda
		(VariableList (Variable "$h") (Variable "$t"))
		(PutLink
			(VariableList (Variable "$head") (Variable "$tail"))
			into-form
			(List (Variable "$h") (Variable "$t")))))

; Lets try it out. Does it work? Yes.
; (cog-execute!
(define mk-edge
	(ExecutionOutput
		(DefinedSchemaNode "make-an-edge")
		(List (Concept "X") (Concept "Y"))))

; --------------------------------------------------------------

; The input DAG edges that we search for
(define get-form
	(Inheritance (Variable "$head") (Variable "$tail")))

; Wrap it up in a function
(DefineLink
	(DefinedSchemaNode "get-the-tail")
	(Lambda
		(Variable "$head")
		(MeetLink
			(TypedVariable (Variable "$tail") (Type 'ConceptNode))
			get-form)))

; Does it work as expected? Yes.
;(cog-execute!
(define get-tl
	(ExecutionOutput
		(DefinedSchemaNode "get-the-tail")
		(List (Concept "A"))))

; --------------------------------------------------------------

; Can we chain them together? Yes we can.
(DefineLink
	(DefinedSchemaNode "rewrite-one")
	(Lambda
		(Variable "$hd")
		(ExecutionOutput
			(DefinedSchemaNode "make-an-edge")
			(List
				(Variable "$hd")
				(ExecutionOutput
					(DefinedSchemaNode "get-the-tail")
					(List (Variable "$hd")))))))

; Does it work? Yes it does.
; (cog-execute!
(define rw-one
	(ExecutionOutput
		(DefinedSchemaNode "rewrite-one")
		(List (Concept "A"))))

; --------------------------------------------------------------

; Do nothing.
(DefineLink
	(DefinedSchemaNode "no-op")
	(Lambda
		(VariableList (Variable "$hd") (Variable "$out"))
		(List (Variable "$hd") (Variable "$out"))))

; A complicated test.
(DefineLink
	(DefinedSchemaNode "test-rewrite")
	(Lambda
		(VariableList (Variable "$hd") (Variable "$out"))
		(ExecutionOutput
			(DefinedSchemaNode "no-op")
			(List
				(ExecutionOutputLink
					(DefinedSchema "get-the-tail")
					(List
						(Variable "$hd")))
				(ExecutionOutput
					(DefinedSchemaNode "make-an-edge")
					(List
						(Variable "$hd")
						(Variable "$out")))))))


; Does it work?
; (cog-execute!
(define nest
	(ExecutionOutput
		(DefinedSchema "test-rewrite")
		(List (Concept "A") (Concept "root"))))

; --------------------------------------------------------------

; Define a recursive tree-walker. It not only recurses, it reverses,
; tracing a path from each leaf, back up to the root. The return is
; a set-link, each element a path from leaf to root.
;
; This used to work, but is now broken/sabotaged. The reason for the
; sabotage is an attempt to remove all the special-case treatment for
; SetLink that has long been a thorn in the side: See issue #2911
; https://github.com/opencog/atomspace/issues/2911
;
; The get-tail function above uses MeetLink to return a UnisetValue
; (instead of the old SetLink) but it gets difficult to flow that
; value throught this nested recursive definition, as currently
; designed. Part of this is due to an old, flawed design for the
; ExecutionOutputLink, and the other part is the ugly, nasty need
; to beta-reduce the LambdaLinks, which causes no end of issues.
; Perhaps the ValueShimLink could be gainfully deployed, I dunno.
; This is a hard nut to crack, and currently a low priority, so
; I write this note and leave it at that.

(DefineLink
	(DefinedSchemaNode "reversive-rewrite")
	(Lambda
		(VariableList (Variable "$hd") (Variable "$out"))
		(Cond
			(Equal (SizeOf (Variable "$hd")) (Number 0))
			(Variable "$out")
			(ExecutionOutput
				(DefinedSchemaNode "reversive-rewrite")
				(List
					(ExecutionOutputLink
						(DefinedSchema "get-the-tail")
						(List
							(Variable "$hd")))
					(ExecutionOutput
						(DefinedSchemaNode "make-an-edge")
						(List
							(Variable "$hd")
							(Variable "$out"))))))))


; (cog-execute!
(define reversive
	(ExecutionOutput
		(DefinedSchema "reversive-rewrite")
		(List (Concept "A") (Concept "root"))))

; What the above generates, when executed.
(define reversive-result
	(SetLink
		(EdgeLink (PredicateNode "yikes") (ListLink
			(ConceptNode "F")
			(EdgeLink (PredicateNode "yikes") (ListLink
				(ConceptNode "B")
				(EdgeLink (PredicateNode "yikes") (ListLink
					(ConceptNode "A")
					(ConceptNode "root")))))))
		(EdgeLink (PredicateNode "yikes") (ListLink
			(ConceptNode "D")
			(EdgeLink (PredicateNode "yikes") (ListLink
				(ConceptNode "C")
				(EdgeLink (PredicateNode "yikes") (ListLink
					(ConceptNode "B")
					(EdgeLink (PredicateNode "yikes") (ListLink
						(ConceptNode "A")
						(ConceptNode "root"))))))))))
)

; --------------------------------------------------------------

; Unwrap a SetLink, and turn it into a ListLink.
(DefineLink
	(DefinedSchemaNode "unwrap")
	(Lambda
		(VariableList (Variable "$set"))
		(Cond
			(Equal (Number 0) (SizeOf (Query (Glob "$elts")
				(Equal (Variable "$set") (Set (Glob "$elts")))
				(List (Glob "$elts")))))
			(Variable "$set")
			(Query (Glob "$elts")
				(Equal (Variable "$set") (Set (Glob "$elts")))
				(List (Glob "$elts"))))))

; (cog-execute!
(define unwrap-set
	(ExecutionOutput
		(DefinedSchema "unwrap")
		(Set (Concept "X") (Concept "Y"))))

(define unwrap-singleton
	(ExecutionOutput
		(DefinedSchema "unwrap")
		(Concept "X")))

(define unwrap-natural
	(ExecutionOutput
		(DefinedSchema "unwrap")
		(Meet (TypedVariable (Variable "$x") (Type 'ConceptNode))
				(Inheritance (Concept "B") (Variable "$x")))))

; A defined Lambda, in atomese.
(DefineLink
	(DefinedSchemaNode "make-a-tree")
	(Lambda
		(VariableList (Variable "$h") (Variable "$set"))
		(PutLink
			(VariableList (Variable "$head") (Variable "$tail"))
			into-form
			(List (Variable "$h")
				(ExecutionOutput
					(DefinedSchema "unwrap")
					(Variable "$set"))))))

; Lets try it out. Does it work? Yes.
; (cog-execute!
(define mk-tree
	(ExecutionOutput
		(DefinedSchemaNode "make-a-tree")
		(List (Concept "head") (Set (Concept "X") (Concept "Y") (Concept"Z")))))

; Lets try it out on a natural set. Does it work? No...
(define mk-tree-indirect
	(ExecutionOutput
		(DefinedSchemaNode "make-a-tree")
		(List (Concept "B")
			(Meet (TypedVariable (Variable "$x") (Type 'ConceptNode))
				(Inheritance (Concept "B") (Variable "$x"))))))


; Define a recursive tree-walker. Unlike the above, this does
; not reverse the order of the edges.
; XXX FIXME, this does not quite work as one might naively expect,
; because the search results are expanded combinatorially, instead
; of being kept in branching-tree form.
(DefineLink
	(DefinedSchemaNode "recursive-rewrite")
	(Lambda
		(VariableList (Variable "$hd"))
		(Cond
			; If there's no tail, then return head.
			(Equal (Set)
				(ExecutionOutputLink
					(DefinedSchema "get-the-tail")
					(List
						(Variable "$hd"))))
			(Variable "$hd")

			; Else make an edge connecting head and tail.
			(ExecutionOutput
				(DefinedSchemaNode "make-a-tree")
				(List
					(Variable "$hd")
					(ExecutionOutput
						(DefinedSchemaNode "recursive-rewrite")
						(List
							(ExecutionOutputLink
								(DefinedSchema "get-the-tail")
								(List
									(Variable "$hd"))))))))))

; (cog-execute!
(define recursive
	(ExecutionOutput
		(DefinedSchema "recursive-rewrite")
		(List (Concept "A"))))

*unspecified*
