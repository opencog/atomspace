;
; filter-strings.scm -- Process streams containing StringValue
;
; Filtering compares an input stream to a fixed pattern, accepting or
; rejecting a token in the input stream based on whether the pattern
; matches or not. The pattern is encoded in Atomese. This presents a
; problem, if the item to be matched is a StringValue, because Values
; cannot be placed directly into Link Atoms.
;
; This example shows how to work around this issue, by using the
; StringOfLink to encode a StringValue in a pattern.

(use-modules (opencog) (opencog exec))

; A mockup of a stream of data to be filtered.
(define stream (LinkValue
	(LinkValue (StringValue "/usr") (StringValue "dir"))
	(LinkValue (StringValue "/usr/lib") (StringValue "dir"))
	(LinkValue (StringValue "/etc") (StringValue "dir"))
	(LinkValue (StringValue "/etc/motd") (StringValue "reg"))
	(LinkValue (StringValue "/dev/sda") (StringValue "block"))
	(LinkValue (StringValue "/dev/tty0") (StringValue "char"))))

; Place the stream where it can be found.
(set-value! (Anchor "rock") (Predicate "key") stream)

; A search pattern to filter the stream.
(define find-files
	(Filter
		(Rule
			(Variable "$filename")
			(LinkSignature (Type 'LinkValue)
				(Variable "$filename")
				(StringOf (Type 'StringValue) (Node "reg")))
			(Edge
				(Predicate "is-a file URL")
				(StringOf (Type 'ItemNode) (Variable "$filename"))))
		(ValueOf (Anchor "rock") (Predicate "key"))))

(cog-execute! find-files)

