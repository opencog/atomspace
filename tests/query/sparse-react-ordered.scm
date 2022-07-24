;
; sparse-react-ordered.scm
;
; Same as sparse-react.scm but using ordered links.
;
(use-modules (opencog))
(use-modules (opencog exec))

; (load-from-path "chem_types.scm")

; This defines an esterification reaction.
(define estero
	(BindLink
		; Variable declaration
		(VariableList

			; Typed variables, specifying specific atoms.
			(TypedVariable (Variable "$carboxyH1") (Type 'H))
			(TypedVariable (Variable "$carboxyO1") (Type 'O))
			(TypedVariable (Variable "$carboxyC1") (Type 'C))
			(TypedVariable (Variable "$carboxyO2") (Type 'O))

			(TypedVariable (Variable "$hydroxH1") (Type 'H))
			(TypedVariable (Variable "$hydroxO1") (Type 'O))

			; Untyped variables that will match to anything
			(Variable "carboxy moiety")
			(Variable "hydroxy moiety")
			(Glob "rest of carboxy")
			(Glob "rest of hydroxy")
		)
		; Premise: Functional groups found in some educts
		(AndLink
			; Look for carboxyl group
			(Molecule
				(DBo (Variable "$carboxyC1") (Variable "$carboxyO2"))
				(SBo (Variable "$carboxyC1") (Variable "$carboxyO1"))
				(SBo (Variable "$carboxyO1") (Variable "$carboxyH1"))
				(SBo (Variable "$carboxyC1") (Variable "carboxy moiety"))

				; Globs match one or more bonds.  To match zero,
				; change the lower bound by declaring it lik this:
				; (TypedVariable (Glob "rest of carboxy")
				;     (Interval (Number 0) (Number -1)))
				(Glob "rest of carboxy")
			)

			; The above will happily match `$carboxyO1` and `carboxy moiety`
			; to the same atom. But we don't want that, so demand that
			; they be distinct.
			(Not (Identical (Variable "$carboxyO1") (Variable "carboxy moiety")))

			; Look for hydroxyl group
			(Molecule
				(SBo (Variable "$hydroxO1") (Variable "$hydroxH1"))
				(SBo (Variable "$hydroxO1") (Variable "hydroxy moiety"))
				(Glob "rest of hydroxy")
			)

			; The above will match the OH's in both groups.
			; They should be kept distinct.
			(Not (Identical (Variable "$carboxyO1") (Variable "$hydroxO1")))
			(Not (Identical (Variable "$carboxyH1") (Variable "$hydroxH1")))
		)
		; Clause: Formation of products
		(AndLink
			; Produce ester
			(Molecule
				(DBo (Variable "$carboxyC1") (Variable "$carboxyO2"))
				(SBo (Variable "$carboxyC1") (Variable "$carboxyO1"))

				(SBo (Variable "$carboxyC1") (Variable "carboxy moiety"))
				(Glob "rest of carboxy")

				(SBo (Variable "$carboxyO1") (Variable "hydroxy moiety"))
				(Glob "rest of hydroxy")
			)
			; Produce water
			(Molecule
				(SBo (Variable "$hydroxO1") (Variable "$carboxyH1"))
				(SBo (Variable "$hydroxO1") (Variable "$hydroxH1"))
			)
		)
	)
)

; ------------------------------------------------
; Populate the AtomSpace with some contents.
;
; Carboxyl group
(Molecule
	(DBo (C "the carboxyl carb") (O "oxy two"))
	(SBo (C "the carboxyl carb") (O "oxy one"))
	(SBo (O "oxy one") (H "carboxyl proton"))
	(SBo (C "the carboxyl carb") (Fe "carbox R"))
	; Some nonsense moiety, for pattern matching only.
	(SBo (Fe "carbox R") (Ni "more carbox junk"))
)

; A hydroxyl group
(Molecule
	(SBo (O "hydroxyl oxy") (H "hydroxyl proton"))

	; Another nonsense moiety, for pattern matching
	(SBo (C "hydroxyl carbon") (O "hydroxyl oxy"))
	(SBo (C "hydroxyl carbon") (Zn "hydrox R"))
	(SBo (Zn "hydrox R") (Cu "junk hydrox moiety"))
)

; ------------------------------------------------
