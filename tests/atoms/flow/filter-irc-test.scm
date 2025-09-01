;
; filter-fail.scm
;
(use-modules (opencog) (opencog exec))


; Private messages have the form:
;    (LinkValue
;       (StringValue "linas")
;       (StringValue "echobot")
;       (StringValue "bunch o text")))
;
; Public messages have the form:
;    (LinkValue
;       (StringValue "linas")
;       (StringValue "#opencog")
;       (StringValue "bunch o text")))
;
; The difference between the two is the target, in the second location.

(cog-set-value! (Anchor "IRC Bot") (Predicate "echo")
	(LinkValue
		(LinkValue
			(StringValue "linas")
			(StringValue "echobot")
			(StringValue "bunch o text"))))

; The filter below, when executed, will pull in a single message,
; pattern match it, and write a private reply to the sender.
; More correctly, executing `make-private-reply` will get one message,
; and rewrite it; but that's it it doesn't send. The private-echo
; below will read-modify-write.

(define make-private-reply
	(Filter
		(Rule
			(VariableList
				(Variable "$from") (Variable "$to") (Variable "$msg"))
			(LinkSignature (Type 'LinkValue)
				(Variable "$from") (Variable "$to") (Variable "$msg"))
			(LinkSignature (Type 'LinkValue)
				(Item "PRIVMSG")
				(Variable "$from")
				(Item "you said: ")
				(Variable "$msg")))
		(ValueOf (Anchor "IRC Bot") (Predicate "echo"))))


; Try it, once
(cog-execute! make-private-reply)

(cog-set-value!
	(Anchor "IRC Bot") (Predicate "bot-name") (StringValue "echobot"))

(define is-pub?
	(Filter
		(Rule
			(VariableList
				(Variable "$from") (Variable "$to") (Variable "$msg"))
			(LinkSignature (Type 'LinkValue)
				(Variable "$from") (Variable "$to") (Variable "$msg"))
			(LinkSignature (Type 'LinkValue)
				(Cond
					(Equal (Variable "$to")
						(ValueOf (Anchor "IRC Bot") (Predicate "bot-name")))
					(Item "private message")
					(Item "public message"))))
		(ValueOf (Anchor "IRC Bot") (Predicate "echo"))))

(cog-execute! is-pub?)


; The End. That's all, folks!
; -------------------------------------------------------
