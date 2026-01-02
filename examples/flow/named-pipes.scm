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

; The stream that is associated with the name can be gotten by executing
; the NameNode:
(cog-execute! (Name "words"))

(define xform
	(Filter
		(Rule
			(Variable "word")
			(Variable "word")
			(LinkSignature (Type 'LinkValue)
				(Concept "You said: ") (Variable "word")))
		(Name "words"))) 

(cog-execute! xform)

; Concatenate


	(LinkSignature (Type 'FlatStream)
		(LinkSignature (Type 'StringValue) (Concept "do-da"))
		(LinkSignature (Type 'StringValue) (Concept "zippity do-dah day"))))
)

(define string-from-node
	(cog-value (Anchor "anch") (Predicate "strkey")))
(format #t "Got string from node ~A\n" string-from-node)

; -----------

; ----------------- That's all, Folks! The End! -----------------
