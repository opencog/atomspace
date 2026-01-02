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
; Give a name to a source of random numbers. The RandomNumberLink
; generates random numbers between a lower and upper bound every time
; it is executed. Thus, for example, the following generates random
; numbers betwen five and ten:

(cog-execute! (RandomNumber (Number 5) (Number 10)))
(cog-execute! (RandomNumber (Number 5) (Number 10)))
(cog-execute! (RandomNumber (Number 5) (Number 10)))

; It is given a name by declaration:
(Pipe
	(Name "five-n-dime store")
	(RandomNumber (Number 5) (Number 10)))

; Note that the above does NOT have to be executed to have the
; name declaration to take effect.

; The NameNode is executable. Executing it yeilds the same results as
; executing the thing that it names:
(cog-execute! (Name "five-n-dime store"))
(cog-execute! (Name "five-n-dime store"))
(cog-execute! (Name "five-n-dime store"))

; -----------
; The above could have been acheived in a far more brutish and indirect
; fashion, using the SetValueLink and the ValueOfLink.

 (cog-execute!
	 (SetValue
		 (Anchor "strip mall")
		 (Predicate "Dollar General")
		 (LinkSignature
			 (Type 'FormulaStream)
			 (RandomNumber (Number 1) (Number 2)))))

; The call to `cog-execute!` is needed to have the SetValue take effect.
; the name is a compound name: an Atom (the Anchor) and a key on that
; Atom (the PredicateNode).  Setting the value would normally just
; dereference the random number generator; to keep it flowing, it needs
; to be converted to a stream, with the LinkSignatureLink.  Its clearly
; much more verbose tthan simply using the PipeLink.

; To get samples from the stream, ValueOfLink is used:

 (cog-execute!
	 (ValueOf
		 (Anchor "strip mall")
		 (Predicate "Dollar General")))

; -----------
; A much more complex example.
; The ability to process streams of words is desriable. Thus, this demo
; shows how this can be done. It is a bit painfully over-wrought, as
; juggling such streams is not an entirely easy matter.
;
; Start by declaring a static data source.  Dynamic data sources ae
; possible, by using one of the sensory nodes to read from a file or
; some social media feed; but this is outside the scop of this demo.
;
; Just to make it a little bit interesting, the collection of strings
; are obtained as the output of a LinkSignature. It is used to convert
; ConceptNodes (which are Atoms) to StringValues (which are not).
;
; The PipeLink just attaches a name to this static data source.
; Note that this is just a declaration; nothing is being executed or
; processed, yet.

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
; of items (mostly because there is no particular difference between
; transpose and concatenate, when everything is a vector.)
(cog-execute! (TransposeColumn (Name "words")))

; A more complicated and awkward embedding of the list of words into
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
