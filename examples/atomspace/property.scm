;
; property.scm -- Designing Atoms with properties.
;
; Many knowledge representation systems want to view the world in
; terms of objects that have properties on them. This can already
; be done quite easily with EvaluationLinks and Predicates.
; But what if one wants a strict guarantee that there is only one
; property of a given name, and then change it time to time?
;
; The StateLink can be used to set "state" in the atomspace; see
; the `state.scm` file for a example.  States can be thought of as
; "key-value" pairs in the atomspace; for a give key, there can only
; ever be one value. Keys and values can be any atoms whatever. By
; using a key that combines an atom with a property name, one can
; implement a "property" on that atom.
;
; This example shows how to set properties, and three different ways
; of getting a named property on a atom.

(use-modules (opencog) (opencog exec))

; Two different atoms, to which a number will be associated.
(Concept "asdf")
(Concept "qwerty")

; The name of the property that will be attached to the above.
(Predicate "truthiness")

; The StateLink associates a property to a atom. In this case, the
; property is a number. The "key" is just a list, combining the atom
; and the property name.
(State (List (Concept "asdf") (Predicate "truthiness"))
	(Number 0.5))

(State (List (Concept "qwerty") (Predicate "truthiness"))
	(Number 0.5))

; Properties can be fetched simply by performing a get:
(cog-execute!
	(Get
		(State (List (Concept "qwerty") (Predicate "truthiness"))
			(Variable "$n"))))

; Changing a property is easy: this bumps up the value to 0.6
(State (List (Concept "qwerty") (Predicate "truthiness"))
	(Number 0.6))

; The getter cane be abstracted away, so that the use of the StateLink
; is hidden from view.  Below, the DefineLink is used to define a schema
; called "get property", which takes two arguments: an atom name, and
; the property.  When executed, the schema returns the property value.
(DefineLink
	(DefinedSchema "get property")
	(Lambda
		(VariableList (Variable "$atom") (Variable "$property"))
		(Get
			(Variable "$n")
			(State (List (Variable "$atom") (Variable "$property"))
				(Variable "$n"))
			)))

; Call the schema defined above.  It should return 0.6 as the value.
(cog-execute!
	(ExecutionOutput
		(DefinedSchema "get property")
		(List (Concept "qwerty") (Predicate "truthiness"))
	))

; --------
; The above example used the rather awkward atom
;     (List (Concept "qwerty") (Predicate "truthiness"))
; which is perhaps an example of "really bad knowledgebase design".
; A better design might have been
;     (Evaluation (Predicate "truthiness") (List (Concept "qwerty")))
; or even
;     (Inheritance (Predicate "truthiness") (Concept "qwerty"))
;
; All that the StateLink is doing is associating a property to an
; atom. In this example, the NumberNode was .. a number; it could be
; interpreted as the degree of membership in a fuzzy set. Or it
; could be something else. It doesn't have to be a number.
;
; The TruthValue is a much better way of representing truth in the
; Atomspace. This is covered in the next example.
;
; NumberNodes are a terribly inefficient way of representing numbers
; in the Atomspace.  A much, much better way are the Values, in the
; upcoming example.
