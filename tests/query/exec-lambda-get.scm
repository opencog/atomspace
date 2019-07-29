;
; execu-lambda-get.scm -- copy of demo `property.scm`
;                      -- Designing Atoms with properties.
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

; The StateLink associates a property to a atom. In this case, the
; property is a number. The "key" is just a list, combining the atom
; and the property name.
(State (List (Concept "asdf") (Predicate "truthiness"))
	(Number 0.5))

(State (List (Concept "qwerty") (Predicate "truthiness"))
	(Number 0.5))

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
(define exo
	(ExecutionOutput
		(DefinedSchema "get property")
		(List (Concept "qwerty") (Predicate "truthiness"))
	))

; --------
