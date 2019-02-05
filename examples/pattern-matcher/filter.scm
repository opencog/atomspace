;
; filter.scm -- Filtering to remove unwanted entries in a set.
;
; The PutLink can be used to filter a set of atoms. This filtering
; is accomplished by using the built-in type-checking facilities
; that accompany any LambdaLink.
;
; See also the `map.scm` example, for more general mapping of sets and
; lists.
;

(use-modules (opencog) (opencog exec))

; Define a PutLink that will keep ConceptNodes, and ignore the rest.
(define filter-it
	(Put
		; Declare a single variable: The variable must be a ConceptNode.
		(TypedVariable (Variable "%x") (Type "ConceptNode"))

		; The body of the PutLink is just the variable itself.
		(Variable "%x")

		; This is the set of things that will be filtered.
		(Set
			(Number     "42")
			(Concept    "foo")
			(Predicate  "biffle")
			(Evaluation (Predicate "foo") (Concept "thingy"))
			(Schema     "finagle")
			(Concept    "bar"))))

; Now, perform the actual filtering:
(cog-execute! filter-it)

; It is expected that the above returns just two items:
(Set
	(Concept "foo")
	(Concept "bar"))

; Similar to the above, but this time, we accept only EvaluationLinks.
(define filter-links
	(Put
		; Declare a single variable: The variable must be an
		; EvaluationLink.
		(TypedVariable (Variable "%x") (Type "EvaluationLink"))

		; The body of the put is just the variable.
		(Variable "%x")

		; This is the set of things that will be filtered.
		(Set
			(Number     "42")
			(Concept    "foo")
			(Predicate  "biffle")
			(Evaluation (Predicate "foo") (Concept "thingy"))
			(Schema     "finagle")
			(Concept    "bar"))))

; Now, perform the actual filtering:
(cog-execute! filter-links)
