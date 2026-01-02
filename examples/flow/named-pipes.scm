;
; named-pipes.scm -- Assigning names to data streams.
;
; Data processing networks can be wired up by assigning names to
; specific data streams.  This allows the output of one processing
; stage to be tagged with a name, so that it can be routed as input
; to another stage. The NameNode provides that name; the PipeLink is
; the mechanism used to attach a name to a data stream.

(use-modules (opencog))

; -----------
; First example.

; Give a name to a source data streamm. This is a static source. Just to
; make it a little bit interesting, it is the output of a LinkSignature.
; The LinkSginature generates some Values, when executed, by converting
; the strings (stored as ConceptNodes) to StringValues. It is a stand-in
; for some data source, that generates data, and can be given a name.
; Note that this is a static declaration. Nothing is executed (yet.)

(PipeLink
	(NameNode "words")
	(LinkSignature (Type 'LinkValue)
		(LinkSignature (Type 'StringValue) (Concept "my-oh-my"))
		(LinkSignature (Type 'StringValue) (Concept "do-da"))
		(LinkSignature (Type 'StringValue) (Concept "zip-a-dee do-dah day"))))

; Execution of the named stream is the same as execution of the stream
; that is named. That is, both behave the same way; executing one is the
; same as executing the other.
(cog-execute! (Name "words"))

; The TransposeColumn provides a generic utility for concatenating lists
; of itmes (mostly because there is no particular difference between
; transpose and concatenate, when everything is a vector.)
(cog-execute! (TransposeColumn (Name "words")))

; A more complicated and awkward empedding of the list of words into
; sentence fragments.
(cog-execute!
	(Filter
		(Rule
			(Variable "word")
			(Variable "word")
			(LinkSignature (Type 'LinkValue)
				(LinkSignature (Type 'StringValue) (Concept "You said: "))
				(Variable "word")))
		(Name "words")))

; The TransposeColumn can be used to provide a kind of concatenation:
(cog-execute!
	(Filter
		(Rule
			(Variable "word")
			(Variable "word")
			(TransposeColumn
				(LinkSignature (Type 'LinkValue)
					(LinkSignature (Type 'StringValue) (Concept "You said: "))
					(Variable "word"))))
		(Name "words")))

; ----------------- That's all, Folks! The End! -----------------
