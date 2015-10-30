;
; aracana-numeric.scm
;
; Some squonky numeric hacking about.
; Get the current time, see how much time has elapsed, and
; return true or false, depending on the elapsed time.

(use-modules (opencog) (opencog exec))

(StateLink (SchemaNode "start-interaction-timestamp") (NumberNode 0))
(StateLink (SchemaNode "current expression duration") (NumberNode 2.0)) ; in seconds

;; line 757, timestamp
(DefineLink
	(DefinedSchemaNode "set timestamp")
	(PutLink
		(StateLink (SchemaNode "start-interaction-timestamp")
			(VariableNode "$x"))
		(TimeLink)))

(DefineLink
	(DefinedSchemaNode "get timestamp")
	(GetLink
		(StateLink (SchemaNode "start-interaction-timestamp")
			(VariableNode "$x"))))

;; Evaluate to true, if an expression should be shown.
;; line 933, should_show_expression()
(DefineLink
	(DefinedPredicateNode "Time to change expression")
	(GreaterThanLink
		(MinusLink
			(TimeLink)
			(DefinedSchemaNode "get timestamp"))
		(GetLink (StateLink (SchemaNode "current expression duration")
			(VariableNode "$x"))) ; in seconds
	))
