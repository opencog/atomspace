;
; type-signature.scm -- Using type signatures and type constructors.
;
; A demonstration of using type signatures during pattern matching.
; Type signatures are a way of specifying the type of a hypergraph.
; This can be used to restrict the search space during a pattern
; match: by default, types are checked during a search, and variable
; groundings must respect the variable type.
;
(use-modules (opencog) (opencog exec))
(use-modules (opencog type-utils))

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
(Evaluation
	(PredicateNode "foo")
	(List (ConceptNode "bingo") (ConceptNode "yes!")))

(Evaluation
	(AnchorNode "bar")
	(List (ConceptNode "hurrah") (ConceptNode "yay!")))

(Evaluation
	(ConceptNode "baz")
	(List (ConceptNode "oops") (ConceptNode "Oh no, Mr. Bill!")))

; A search pattern that looks for predicates or grounded predicates.
(define predicate-search
	(Get
		(TypedVariable
			(Variable "$x")
			(Signature
				(Evaluation
					(TypeChoice
						(Type "PredicateNode")
						(Type "AnchorNode"))
					(List
						(Type "ConceptNode") (Type "ConceptNode")))))
		(And (Variable "$x"))))

(cog-execute! predicate-search)

; =============================================================

; The cog-value-is-type? function allows signatures to be
; directly validated. Consider these examples:

; Should evaluate to true.
(cog-value-is-type?
	(Signature (Inheritance (Concept "foo") (Type "ConceptNode")))
	(Inheritance (Concept "foo") (ConceptNode "bar")))

; Should evaluate to false.
(cog-value-is-type?
	(Signature (Inheritance (Concept "foo") (Type "ConceptNode")))
	(Inheritance (Concept "failure-mode") (ConceptNode "bar")))

; We can define new types, too.
(DefineLink
	(DefinedType "My foo type")
	(Signature (Inheritance (Concept "foo") (Type "ConceptNode"))))

; Should evaluate to true
(cog-value-is-type?
	(DefinedType "My foo type")
	(Inheritance (Concept "foo") (ConceptNode "bar")))

; Should evaluate to false.
(cog-value-is-type?
	(DefinedType "My foo type")
	(Inheritance (Concept "failure-mode") (ConceptNode "bar")))
