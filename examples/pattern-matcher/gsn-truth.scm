;
; gsn-truth.scm -- Altering TruthValues with GroundedSchemaNode
;
; One of the most basic usages of the GroundedSchemaNode is to
; recompute the TruthValue on an atom, once its been found.
;
(use-modules (opencog))

; A query to find all humans
(define human
	(Get
		; This is a pattern that will be matched ...
		; The result of grounding this will be all things
		; that could ever possibly be $H
		(Inheritance (Variable "$H") (Concept "human"))))


; A re-write rule that, when it finds one graph, it creates another.
; Here, for each $H that is human, asserts that $H is an animal.
(define human-implies-animal
	(Bind
		; This is the pattern that will be matched ...
		(Inheritance (Variable "$H") (Concept "human"))

		; This is the hypergraph that will be created if the
		; above pattern is found.
		(Inheritance (Variable "$H") (Concept "animal"))))

; A re-write rule, like the above, except that, when it triggers,
; it also computes a custom truth value.
(define human-implies-animal-stv
	(Bind
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
				; The schema will take the truth value from this link ...
				(Inheritance (Variable "$H") (Concept "human"))

				; .. and will set the truth value here, after scaling by 0.3.
				(Inheritance (Variable "$H") (Concept "animal"))))))

; Return a new truth value, where the strength was multiplied by 'val',
; but the confidence was copied without change.
(define (scale-tv-strength val tv)
	(SimpleTruthValue (* val (cog-tv-mean tv)) (cog-tv-confidence tv)))

; Define a formula that computes a truth value for atom2 based on atom1's stv.
(define (modify-stv atom1 atom2)
	; Set the strength of the truth value on atom hb
	; to be just 0.3 of the strength of atom ha.
	(cog-set-tv! atom2 (scale-tv-strength 0.3 (cog-tv atom1)))
	atom2	; return atom hb
)

; Some data to populate the AtomSpace:
(InheritanceLink (stv 1 0.99)	; a non-zero truth value is needed!
	(Concept "Ben")
	(Concept "human"))

(InheritanceLink (stv 1 0.99)	; a non-zero truth value is needed!
	(Concept "Linas")
	(Concept "human"))

;;;; Run the Pattern-Matcher by invoking any of the following.

; (cog-execute! human)
; (cog-execute! human-implies-animal)
; (cog-execute! human-implies-animal-stv)

;;;; Expected output for each case above:

; (SetLink
;    (Concept "Ben")
;    (Concept "Linas"))
;
; (SetLink
;    (InheritanceLink (Concept "Linas") (Concept "animal"))
;    (InheritanceLink (Concept "Ben") (Concept "animal")))
;
; (SetLink
;    (InheritanceLink (stv 0.3 0.99)
;       (Concept "Linas")
;       (Concept "animal"))
;    (InheritanceLink (stv 0.3 0.99)
;       (Concept "Ben")
;       (Concept "animal")))
