;
; sparse-react.scm
;
; Simple chemistry demo. The chemistry here is wildoly incorrect,
; for several reasons. The biggest is missing constraints.
;
(use-modules (opencog))
(use-modules (opencog exec))

; (load "test_types.scm";)

; This defines an esterification reaction.
(define est (Set))
(define esterification
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
				(DB (Variable "$carboxyC1") (Variable "$carboxyO2"))
				(SB (Variable "$carboxyC1") (Variable "$carboxyO1"))
				(SB (Variable "$carboxyO1") (Variable "$carboxyH1"))
				(SB (Variable "$carboxyC1") (Variable "carboxy moiety"))

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
				(SB (Variable "$hydroxO1") (Variable "$hydroxH1"))
				(SB (Variable "$hydroxO1") (Variable "hydroxy moiety"))
				(Glob "rest of hydroxy")
			)
		)
		; Clause: Formation of products
		(AndLink
			; Produce ester
			(Molecule
				(DB (Variable "$carboxyC1") (Variable "$carboxyO2"))
				(SB (Variable "$carboxyC1") (Variable "$carboxyO1"))

				(SB (Variable "$carboxyC1") (Variable "carboxy moiety"))
				(Glob "rest of carboxy")

				(SB (Variable "$carboxyO1") (Variable "hydroxy moiety"))
				(Glob "rest of hydroxy")
			)
			; Produce water
			(Molecule
				(SB (Variable "$hydroxO1") (Variable "$carboxyH1"))
				(SB (Variable "$hydroxO1") (Variable "$hydroxH1"))
			)
		)
	)
)

; ------------------------------------------------
; Populate the AtomSpace with some contents.
;
; Carboxyl group
(Molecule
	(DB (C "the carboxyl carb") (O "oxy two"))
	(SB (C "the carboxyl carb") (O "oxy one"))
	(SB (O "oxy one") (H "carboxyl proton"))
	(SB (C "the carboxyl carb") (Fe "carbox R"))
	; Some nonsense moiety, for pattern matching only.
	(SB (Fe "carbox R") (Ni "more carbox junk"))
)

; A hydroxyl group
(Molecule
	(SB (O "hydroxyl oxy") (H "hydroxyl proton"))

	; Another nonsense moiety, for pattern matching
	(SB (C "hydroxyl carbon") (O "hydroxyl oxy"))
	(SB (C "hydroxyl carbon") (Zn "hydrox R"))
	(SB (Zn "hydrox R") (Cu "junk hydrox moiety"))
)

; ------------------------------------------------
