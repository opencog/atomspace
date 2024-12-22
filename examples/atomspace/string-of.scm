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
