;
; filter-strings-test.scm -- Unit test for filter-strings.scm example.
;

(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))

(opencog-test-runner)
(define tname "filter-strings-test")
(test-begin tname)

; -----------

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
		(Rule
			(Variable "$filename")

			(LinkSignature (Type 'LinkValue)
				(Variable "$filename")
				(StringOf (Type 'StringValue) (Node "reg")))

			(Edge
				(Predicate "is-a file URL")
				(StringOf (Type 'ItemNode) (Variable "$filename"))))

		(ValueOf (Anchor "rock") (Predicate "key"))))

; Apply the filter to the input stream.
(define fili (cog-execute! find-files))

(test-assert "find-files  rule"
	(equal? fili (LinkValue
		(Edge
			(Predicate "is-a file URL")
			(Item "/etc/motd")))))

; -----------

(test-end tname)
(opencog-test-end)
