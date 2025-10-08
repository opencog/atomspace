;
; gsn-truth.scm -- Altering truth values with GroundedSchemaNode
;
; One of the most basic usages of the GroundedSchemaNode is to
; recompute the truth values on an atom, once its been found.
;
(use-modules (opencog) (opencog exec))

; Define the key used to store truth values (2 floats: mean, confidence)
(define tvkey (Predicate "*-TruthValueKey-*"))

; A query to find all humans
(define human
	(Meet
		; This is a pattern that will be matched ...
		; The result of grounding this will be all things
		; that could ever possibly be $H
		(Inheritance (Variable "$H") (Concept "human"))))


; A re-write rule that, when it finds one graph, it creates another.
; Here, for each $H that is human, asserts that $H is an animal.
(define human-implies-animal
	(Query
		; This is the pattern that will be matched ...
		(Inheritance (Variable "$H") (Concept "human"))

		; This is the hypergraph that will be created if the
		; above pattern is found.
		(Inheritance (Variable "$H") (Concept "animal"))))

; A re-write rule, like the above, except that, when it triggers,
; it also computes a custom float value.
(define human-implies-animal-stv
	(Query
		; This is the pattern that will be matched ...
		(Inheritance (Variable "$H") (Concept "human"))

		(ExecutionOutput
			(GroundedSchema "scm: modify-stv")

			; This is the list of arguments to pass to the formula.
			; Notice that *two* arguments are passed.	The first
			; argument is a repeat of the pattern that was matched,
			; and the second argument is a hypergraph that will be
			; created.
			(List
				; The schema will take the float value from this link ...
				(Inheritance (Variable "$H") (Concept "human"))

				; .. and will set the float value here, after scaling by 0.3.
				(Inheritance (Variable "$H") (Concept "animal"))))))

; Return a new FloatValue, where the first component (mean) was multiplied by 'val',
; but the second component (confidence) was copied without change.
(define (scale-fv-strength val fv)
	(FloatValue (* val (cog-value-ref fv 0)) (cog-value-ref fv 1)))

; Define a formula that computes a FloatValue for atom2 based on atom1's FloatValue.
(define (modify-stv atom1 atom2)
	; Set the strength of the float value on atom2
	; to be just 0.3 of the strength of atom1.
	(cog-set-value! atom2 tvkey
		(scale-fv-strength 0.3 (cog-value atom1 tvkey)))
)

; Some data to populate the AtomSpace:
(cog-set-value!
	(InheritanceLink
		(Concept "Ben")
		(Concept "human"))
	tvkey (FloatValue 1.0 0.99))	; a non-zero float value is needed!

(cog-set-value!
	(InheritanceLink
		(Concept "Linas")
		(Concept "human"))
	tvkey (FloatValue 1.0 0.99))	; a non-zero float value is needed!

;;;; Run the Pattern-Matcher by invoking any of the following.

; (cog-execute! human)
; (cog-execute! human-implies-animal)
; (cog-execute! human-implies-animal-stv)

;;;; Expected output for each case above:

; (QueueValue
;    (Concept "Ben")
;    (Concept "Linas"))
;
; (QueueValue
;    (InheritanceLink (Concept "Linas") (Concept "animal"))
;    (InheritanceLink (Concept "Ben") (Concept "animal")))
;
; (QueueValue
;    (InheritanceLink   ; with FloatValue (0.3 0.99) at tvkey
;       (Concept "Linas")
;       (Concept "animal"))
;    (InheritanceLink   ; with FloatValue (0.3 0.99) at tvkey
;       (Concept "Ben")
;       (Concept "animal")))
