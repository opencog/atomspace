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

(define A (Concept "A"))
(define B (Concept "B"))

; ----------
(define a-implies-b (Implication (Concept "A") (Concept "B")))

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

(define p-implies-q (Implication (Concept "P") (Concept "Q")))
(cog-execute!
	(SetTV
		(Implication (Concept "P") (Concept "Q"))
		(DefinedPredicate "dynamic example")
		(List (Concept "A") (Concept "B"))))

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
