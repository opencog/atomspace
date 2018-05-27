;
; property.scm -- Associating properties with atoms.
;
; The StateLink can be used to set "state" in he atomspace; see
; the state.scm file for a example.  States can be thought of as
; "key-value" pairs in the atomspace; for a give key, there can only
; ever be one value. Keys and values can be any atoms whatever. By
; using a key that combines an atom with a property name, one can
; implement a "property" on that atom.
;
; This example shows how to set properties, and three different ways
; of getting a named property on a atom.

(use-modules (opencog) (opencog exec))

; Two differet atoms, to which a number will be associated.
(ConceptNode "asdf")
(ConceptNode "qwerty")

; The name of the property that will be attached to the above.
(PredicateNode "truthiness")

; The StateLink associates a property to a atom. In this case, the
; property is a number. The "key" is just a list, combining the atom
; and the property name.
(StateLink (ListLink (ConceptNode "asdf") (PredicateNode "truthiness"))
	(NumberNode 0.5))

(StateLink (ListLink (ConceptNode "qwerty") (PredicateNode "truthiness"))
	(NumberNode 0.5))

; Properties can be fetched simply by performing a get:
(cog-execute!
	(GetLink
		(StateLink (ListLink (ConceptNode "qwerty") (PredicateNode "truthiness"))
			(VariableNode "$n"))))

; Changing a property is easy: this bumps up the value to 0.6
(StateLink (ListLink (ConceptNode "qwerty") (PredicateNode "truthiness"))
	(NumberNode 0.6))

; The getter cane be absracted away, so that the use of the StateLink
; is hidde from view.  Below, the DefineLink is used to define a schema
; called "get property", which takes two arguments: an atom name, and
; the property.  When executed, the schema returs the property value.
(DefineLink
	(DefinedSchemaNode "get property")
	(LambdaLink
		(VariableList (VariableNode "$atom") (VariableNode "$property"))
		(GetLink
			(StateLink (ListLink (VariableNode "$atom") (VariableNode "$property"))
				(VariableNode "$n"))
			)))

; Call the schema defined above.  It should return 0.6 as the value.
(cog-execute!
	(ExecutionOutputLink
		(DefinedSchemaNode "get property")
		(ListLink (ConceptNode "qwerty") (PredicateNode "truthiness"))
	))
