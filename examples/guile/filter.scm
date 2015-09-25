;
; Filtering example
;
; The PutLink can be used to filter a set of atoms. This filtering
; is accomplished by using the built-in type-checking facilities
; that accompany any LambdaLink.
;

(use-modules (opencog))
(use-modules (opencog exec))
(use-modules (opencog query))

(define filter-it
	(PutLink
		; Declare a single variable: The variable must be a ConceptNode.
		(TypedVariableLink
			(VariableNode "%x")
			(TypeNode "ConceptNode"))
		; The body of the put is just the variable.
		(VariableNode "%x")

		; This is the set of things that will be filetered
		(SetLink
			(NumberNode "42")
			(ConceptNode "foo")
			(PredicateNode "biffle")
			(EvaluationLink
				(ConceptNode "thingy"))
			(SchemaNode "finagle")
			(ConceptNode "bar"))))

; Now, perform the actual filtering:
(cog-execute! filter-it)

; It is expected that the above returns just two items:
(SetLink
	(ConceptNode "foo")
	(ConceptNode "bar"))
