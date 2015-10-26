
;
; Associating properties with atoms
;

(use-modules (opencog) (opencog exec))

(ConceptNode "asdf")
(ConceptNode "qwerty")

(PredicateNode "truthiness")

(StateLink (ListLink (ConceptNode "asdf") (PredicateNode "truthiness"))
	(NumberNode 0.5))

(StateLink (ListLink (ConceptNode "qwerty") (PredicateNode "truthiness"))
	(NumberNode 0.5))

(cog-execute!
(GetLink
	(StateLink (ListLink (ConceptNode "qwerty") (PredicateNode "truthiness"))
		(VariableNode "$n")))
)

(cog-execute!
(GetLink
	(PutLink
		(VariableList (VariableNode "$atom") (VariableNode "$property"))
		(StateLink (ListLink (VariableNode "$atom") (VariableNode "$property"))
			(VariableNode "$n"))
		(ListLink (ConceptNode "qwerty") (PredicateNode "truthiness"))
	)))


(cog-execute!
(ExecutionOutputLink
	(LambdaLink
		(VariableList (VariableNode "$a") (VariableNode "$p"))
		(GetLink
			(PutLink
				(VariableList (VariableNode "$atom") (VariableNode "$property"))
				(StateLink (ListLink (VariableNode "$atom") (VariableNode "$property"))
					(VariableNode "$n"))
				(ListLink (VariableNode "$a") (VariableNode "$p"))
			)))
	(ListLink (ConceptNode "qwerty") (PredicateNode "truthiness"))
))
