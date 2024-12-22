;
; string-of.scm -- Converting StringValues into Nodes & vice-versa
;
; The FilterLink can be used to rewrite streaming data from one format
; into another ... when that data is a sequence of Links or LinkValues.
; Converting from a StringValue to a Node presents a challenge, because
; there is no way to get "naked strings" in Atomese.
;
; The need to convert stransient StringValue streams into concrete
; Nodes stored in the AtomSpace is fulfilled by the StringOfLink
; It will convert between different Node types, and also between
; Nodes and StringValues.
;
; More abstractly: Given dynamic data sources that are flowing streams
; of strings, such as chat servers or web-page chat interfaces, there
; can be a need to capture some or all of the string stream as Atomese
; Atoms, so that, for example, they become searchable using the standard
; query engine interfaces. To become searchable, the strings need to be
; converted into Nodes, typically ConceptNodes, ItemNodes or WordNodes.
; The StringOfLink provides that conversion.
;
; Conversely, there might be a need to stream the contents of the
; AtomSpace back out to a chat interface; the StringOfLink can provide
; the needed conversion from AtomSpace Nodes to string streams.
;
(use-modules (opencog) (opencog exec))

; Given the (PredicateNode "bar"), create the (ConceptNode "bar")

(define node-from-node
	(cog-execute! (StringOf (Type 'Concept) (Predicate "bar"))))
(format #t "Node from node created ~A\n" node-from-node)

; -----------

; Given a StringValue located somewhere, create a corresponding
; ConceptNode
(cog-set-value! (Anchor "anch") (Predicate "key")
	(StringValue "a" "b" "c"))

(define node-from-string
	(cog-execute! (StringOf (Type 'Concept)
		(ValueOf (Anchor "anch") (Predicate "key")))))

(format #t "Node from string got ~A\n" node-from-string)

; -----------

; Given a ConceptNode, create a corresponding StringValue (which can
; then be flowed to desired targets.)
(cog-execute!
	(SetValue (Anchor "anch") (Predicate "strkey")
		(StringOf (Type 'StringValue)
			(Concept "do-da"))))

(define string-from-node
	(cog-value (Anchor "anch") (Predicate "strkey")))
(format #t "Got string from node ~A\n" string-from-node)

; -----------

; Verify that the conversion flows correctly. For this part of the demo,
; a stream of strings is to be converted into Nodes. That is, the
; AtomSpace acts as a kind of "sink" for the stream, where the flow
; terminates by turning into Nodes. Alternately, the flow "freezes" or
; stops, leaving behind Nodes as the frozen artifact of the data that
; came trhough.
;
(cog-set-value! (Anchor "anch") (Predicate "flokey")
	(LinkValue
		(StringValue "here")
		(StringValue "is")
		(StringValue "a")
		(StringValue "sequence")
		(StringValue "of")
		(StringValue "words")))

; Create a filter that will process the incoming stream, and do the
; string conversion. This uses a rewrite rule to map the flow input
; to Nodes, and then wrap result into an EdgeLink that tags the data
; with its data type.
(define tag-sentence-words
	(Filter
		(Rule
			; The variable declaration for the rule.
			(Variable "$strv")

			; The input acceptance pattern. The rule fires only if
			; the input pattern matches. In this case, the rule
			; matches everything.
			(Variable "$strv")

			; The rewrite to perform. The stream items will be converted
			; into ConceptNodes, and then tagged to indicate their type.
			; (They came from a sentence. They are words.)
			(Edge (Predicate "sentence word")
				(StringOf (Type 'Concept)
					(ValueOf (Variable "$strv")))))

		; The stream source, to which the filter is applied.
		(ValueOf (Anchor "anch") (Predicate "flokey"))))

; Run the stream. Calling cog-execute! will apply the filter to the
; stream.
(cog-execute! tag-sentence-words)

; Where dd those words go? Well, they are now Atoms in the Atompace.
; Conventional AtomSpace processing can now be applied to them.
; The query below will fish out all of th words that have flowed in.
(define query
	(Meet
		(TypedVariable (Variable "$word") (Type 'Concept))
		(Edge (Predicate "sentence word") (Variable "$word"))))

; Perform the query, get the words.
(define observed-words (cog-execute! query))

(format #t "These words were seen: ~A\n" observed-words)

; ----------------- That's all, Folks! The End! -----------------
