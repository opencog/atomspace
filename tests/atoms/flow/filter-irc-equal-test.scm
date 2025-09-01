;
; filer-irc-equal.scm -- Failiing FilterLink.
;
(use-modules (opencog) (opencog exec) (opencog sensory))

; Messages have the form:
;    (LinkValue
;       (StringValue "linas")
;       (StringValue "echobot")
;       (StringValue "bunch o text")))
;

; Is it a public or private message?
; It is private if (Variable "$to") is the name of the bot.
(define is-pub?
	(Cond
		(Equal (Variable "$to")
			(ValueOf (Anchor "IRC Bot") (Predicate "bot-name")))
		(Item "private message")
		(Item "public message")))

; Example: Is the bot being called out?
; A "callout" is a string that starts with the botname, followed
; by a colon. This is an IRC convention, and nothing more.

(define is-callout?
	(Cond
		(Equal
			(ElementOf (Number 0) (Variable "$msg"))
			(ValueOf (Anchor "IRC Bot") (Predicate "bot-name")))
		(Item "calls out the bot")
		(Item "is just a message")))

(define callout-reply
	(list (Item "PRIVMSG") (Variable "$from")
	(Item "Message to ")
	(Variable "$to")
	(Item " is a ")
	is-pub?
	(Item " from ")
	(Variable "$from")
	(Item "that ")
	is-callout?
	(Item ": ")
	(Variable "$msg")))

; Set up required data
; The bot needs to know it's own name.
(cog-set-value! (Anchor "IRC Bot")
	(Predicate "bot-name") (StringValue "echobot"))

; The input
(cog-set-value! (Concept "echobot") (Predicate "*-stream-*")
	(LinkValue
		(LinkValue
			(StringValue "linas")
			(StringValue "echobot")
			(StringValue "bing bang boom bomb"))))

(define do-callout
	(SetValue
		(Concept "echobot")
		(Predicate "*-write-*")
		(Filter
			(Rule
				(VariableList
					(Variable "$from") (Variable "$to") (Variable "$msg"))
				(LinkSignature (Type "LinkValue")
					(Variable "$from") (Variable "$to") (Variable "$msg"))
				(LinkSignature (Type "LinkValue")
					;; callout-reply
					(Item "PRIVMSG")
					(Variable "$from")
					(Item "Message to ")
					(Variable "$to")
					(Item " is a ")
					(Cond
						(Equal
							(Variable "$to")
							(ValueOf
								(Anchor "IRC Bot")
								(Predicate "bot-name")))
						(Item "private message")
						(Item "public message"))
					(Item " from ")
					(Variable "$from")
					(Item "that ")
					(Cond
						(Equal
							(ValueOf
								(Anchor "IRC Bot")
								(Predicate "bot-name"))
							(ElementOf
								(Number "0")
								(Variable "$msg")))
						(Item "calls out the bot")
						(Item "is just a message"))
					(Item ": ")
					(Variable "$msg")))
			(ValueOf
				(Concept "echobot")
				(Predicate "*-stream-*")))))

(define reply (cog-execute! do-callout))

(format #t "Reply is ~A\n" reply)

; The End. That's all, folks!
; -------------------------------------------------------
