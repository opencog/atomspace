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
(cog-set-value! (Anchor "rock") (Predicate "key") stream)

; A search pattern to filter the stream.
(define find-files
	(Filter
		; The rule is applied to each item in the input stream
		(Rule
			; A variable declaratio for the variables used in the rule.
			(Variable "$filename")

			; The pattern to be matched. Look for direntries that are
			; regular files, having file type (StringValue "reg").
			; The StringOfLink is an Atom and can be placed in the
			; pattern. It grabs the Node name, and constructs a
			; a StringValue which is then used in the search pattern.
			(LinkSignature (Type 'LinkValue)
				(Variable "$filename")
				(StringOf (Type 'StringValue) (Node "reg")))

			; The rewrite to be applied to all matching items. In this
			; case, the filename is converted from a StringValue to a
			; concrete Node, an ItemNode, and placed in the AtomSpace.
			; It is also tagged as a file with a Predicate tag. This
			; allows all file URL's in the AtomSpace to be found at
			; some later time, without looking at the external world
			; again. The AtomSpace now "remembers" what was seen.
			(Edge
				(Predicate "is-a file URL")
				(StringOf (Type 'ItemNode) (Variable "$filename"))))

		; The location of the input stream, to which the filter
		; pattern will be applied.
		(ValueOf (Anchor "rock") (Predicate "key"))))

; Apply the filter to the input stream.
(cog-execute! find-files)

; That's all, folks! The End.
