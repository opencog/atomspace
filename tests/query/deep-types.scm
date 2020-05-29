;
; deep-types.scm
; Used for Unit test DeepTypesUTest based on example code.
;
; A demonstration of using type signatures aka "deep types" during
; pattern matching.  Type signatures are a way of specifying the type
; of a hypergraph.  This can be used to restrict the search space during
; a pattrn match: by default, types are checked during a search, and
; variable groundings must respect the variable type.
;
(use-modules (opencog))
(use-modules (opencog exec))
(use-modules (opencog type-utils))

; =============================================================

; The cog-value-is-type? function allows signatures to be
; directly validated. Consider these examples:

; Should evaluate to true.
;(cog-value-is-type?
;   (Signature (Inheritance (Concept "foo") (Type "ConceptNode")))
;   (Inheritance (Concept "foo") (ConceptNode "bar")))

; Should evaluate to false.
;(cog-value-is-type?
;   (Signature (Inheritance (Concept "foo") (Type "ConceptNode")))
;   (Inheritance (Concept "failure-mode") (ConceptNode "bar")))

; We can define new types, too.
(DefineLink
   (DefinedType "My foo type")
   (Signature (Inheritance (Concept "foo") (Type "ConceptNode"))))

; Should evaluate to true
;(cog-value-is-type?
;   (DefinedType "My foo type")
;   (Inheritance (Concept "foo") (ConceptNode "bar")))

; Should evaluate to false.
;(cog-value-is-type?
;   (DefinedType "My foo type")
;   (Inheritance (Concept "failure-mode") (ConceptNode "bar")))

; =============================================================
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
; (cog-execute! get-foo)

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
; (cog-execute! get-foobar)

; =============================================================
; A more complex example

; More data:
(EvaluationLink
	(PredicateNode "foo")
	(ListLink (ConceptNode "bingo") (ConceptNode "yes!")))

(EvaluationLink
	(AnchorNode "bar")
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
						(TypeNode "AnchorNode"))
					(ListLink
						(Type "ConceptNode") (Type "ConceptNode")))))
		(AndLink (Variable "$x"))))

(DefineLink
	(DefinedType "predicate-type")
	(Signature
		(EvaluationLink
			(TypeChoice
				(TypeNode "PredicateNode")
				(TypeNode "AnchorNode"))
			(ListLink
				(Type "ConceptNode") (Type "ConceptNode")))))

(define predicate-search-typed
	(GetLink
		(TypedVariable
			(Variable "$x")
			(DefinedType "predicate-type"))
		(AndLink (Variable "$x"))))

; (cog-execute! predicate-search)
; =============================================================
; A somewhat silly special case involving a constant

(define constant-a
	(Get
		(TypedVariable (Variable "X") (Signature (Concept "A")))
		(Present (Variable "X"))))

; After Holiday in Berlin ...
(define constant-zappa
	(Get
		(TypedVariable (Variable "X") (Signature (Concept "A")))
		(Present
			(Evaluation (Predicate "Aybe Sea")
				(ListLink
					(Variable "X") (Concept "B") (Concept "C"))))))

; Above should find the below.
(Evaluation (Predicate "Aybe Sea")
	(ListLink (Concept "A") (Concept "B") (Concept "C")))

; (cog-execute! constant-zappa)

; =============================================================
; Disconennted components with deep types.

(define deep-disconnect
   (Get
      (VariableList
         (TypedVariable (Variable "X") (Signature (Concept "A")))
         (TypedVariable (Variable "Y") (Signature (Concept "B"))))
      (And
         (Present (Variable "X"))
         (Present (Variable "Y")))))

; (cog-execute! deep-disconnect)

; =============================================================
