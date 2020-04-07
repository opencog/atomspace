;
; dynamic.scm -- test-case version of `flow-formulas.scm` example.
;

(use-modules (opencog) (opencog exec))

(DefineLink
   (DefinedPredicate "has a reddish color")
   (PredicateFormula
      (Minus
         (Number 1)
         (Times
            (StrengthOf (Variable "$X"))
            (StrengthOf (Variable "$Y"))))
      (Times
         (ConfidenceOf (Variable "$X"))
         (ConfidenceOf (Variable "$Y")))))

(define evlnk
	(Evaluation
		(DefinedPredicate "has a reddish color")
		(List (Concept "A") (Concept "B"))))

(define tv-stream (FormulaTruthValue evlnk))

(cog-set-tv! (Concept "A") (stv 0.9 0.2))
(cog-set-tv! (Concept "A") (stv 0.5 0.8))
(cog-set-tv! (Concept "B") (stv 0.314159 0.9))
(display tv-stream) (newline)

; ----------
(define a-implies-b (Implication (Concept "A") (Concept "B")))
(cog-set-tv! a-implies-b tv-stream)

; Take a look at it, make sure that it is actually there.
(cog-tv a-implies-b)

(cog-set-tv! (Concept "A") (stv 0.4 0.2))
(cog-set-tv! (Concept "B") (stv 0.7 0.8))

; ----------
; For example:
(cog-execute!
	(SetTV
		(Implication (Concept "A") (Concept "B"))
		(DynamicFormula
			(Minus
				(Number 1)
				(Times
					(StrengthOf (Concept "A"))
					(StrengthOf (Concept "B"))))
			(Times
				(ConfidenceOf (Concept "A"))
				(ConfidenceOf (Concept "B"))))))

(DefineLink
   (DefinedPredicate "dynamic example")
   (DynamicFormula
      (Minus
         (Number 1)
         (Times
            (StrengthOf (Variable "$X"))
            (StrengthOf (Variable "$Y"))))
      (Times
         (ConfidenceOf (Variable "$X"))
         (ConfidenceOf (Variable "$Y")))))

(cog-execute!
	(SetTV
		(Implication (Concept "A") (Concept "B"))
		(DefinedPredicate "dynamic example")
		(List (Concept "A") (Concept "B"))))

(cog-tv a-implies-b)

; Change the TV on A and B ...
(cog-set-tv! (Concept "A") (stv 0.1 0.9))
(cog-set-tv! (Concept "B") (stv 0.1 0.9))

; -------------------------------------------------------------
(define foo (Concept "foo"))
(define bar (Concept "bar"))
(define akey (Predicate "some key"))
(define bkey (Predicate "other key"))

(cog-set-value! foo akey (RandomStream 5))

; Take a look at what was created.
(cog-value foo akey)
(cog-value->list (cog-value foo akey))

(define fstream (FormulaStream (Plus (Number 10) (ValueOf foo akey))))

(cog-set-value! bar bkey fstream)
(cog-value bar bkey)
(cog-value->list (cog-value bar bkey))

; ------- THE END -------
