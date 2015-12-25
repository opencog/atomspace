;
; A demonstration of using type signatures aka "deep types" during
; pattern matching.  Type signatures are a way of specifying the type
; of a hypergraph.  This can be used to restrict the search space during
; a pattrn match: by default, types are checked during a search, and
; variable groundings must respect the variable type.
;
(use-modules (opencog) (opencog query) (opencog exec))

; Populate the atomspace with some nonsense atoms.
(Inheritance (Concept "foo") (Concept "bingo"))
(Inheritance (Concept "bar") (Concept "bingo"))
(Inheritance (Concept "baz") (Concept "bonk"))

; Define a search that should return only the first inheritance link.
(define get-foo
	(GetLink
		(TypedVariable (Variable "$x")
			(Signature (Inheritance (Concept "foo") (Type "ConceptNode"))))

		; Search the entire atomspace for this.
		(Variable "$x")
	)
)

; Perform the search. Should return only the foo-inheritance.
(cog-execute! get-foo)

; Define a search that should return the first two inheritance links.
(define get-foobar
	(GetLink
		(TypedVariable (Variable "$x")
			(TypeChoice
				(Signature (Inheritance (Concept "foo") (Type "ConceptNode")))
				(Signature (Inheritance (Concept "bar") (Type "ConceptNode")))))

		; Search the entire atomspace for this.
		(Variable "$x")
	)
)

; Perform the search. Should return both foo and bar-inheritance.
(cog-execute! get-foobar)

; =============================================================
; A more complex example

; More data:
(EvaluationLink
	(PredicateNode "foo")
	(ListLink (ConceptNode "bingo") (ConceptNode "yes!")))

(EvaluationLink
	(GroundedPredicateNode "bar")
	(ListLink (ConceptNode "hurrah") (ConceptNode "yay!")))

(EvaluationLink
	(ConceptNode "baz")
	(ListLink (ConceptNode "oops") (ConceptNode "Oh no, Mr. Bill!")))

; A search pattern that looks for predicates or grounded predicates.
(define predicate-search
	(GetLink
		(TypedVariable
			(Variable "$x")
			(Signature
				(EvaluationLink
					(TypeChoice
						(TypeNode "PredicateNode")
						(TypeNode "GroundedPredicateNode"))
					(ListLink
						(Type "ConceptNode") (Type "ConceptNode")))))
		(AndLink (Variable "$x"))))

(cog-execute! predicate-search)
