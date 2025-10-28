;
; dynamic.scm -- test-case version of `flow-formulas.scm` example.
;

(use-modules (opencog) (opencog exec))

; For DynamicUTest::test_predicate_formula()

(define A (Concept "A"))
(define B (Concept "B"))

(define tvkey (Predicate "*-Fake TruthValueKey-*"))
(define (set-fake-tv! ATOM TV) (cog-set-value! ATOM tvkey TV))
(define (fake-tv ATOM) (cog-value ATOM tvkey))

(define (strength-of ATOM) (ElementOf (Number 0) (ValueOf ATOM tvkey)))
(define (confidence-of ATOM) (ElementOf (Number 1) (ValueOf ATOM tvkey)))

(cog-set-value! (Concept "A") tvkey (FloatValue 0.9 0.1))
(cog-set-value! (Concept "B") tvkey (FloatValue 0.8 0.2))

; Was FormulaTruthValue
(define fututv
	(FormulaStream
		(Minus
			(Number 1)
			(Times (strength-of A) (strength-of B)))
		(Times (confidence-of A) (confidence-of B))))

; ----------
; For DynamicUTest::test_formula_define()
(DefineLink
	(DefinedProcedure "has a reddish color")
	(Lambda
		(VariableList (Variable "$X") (Variable "$Y"))
		(FloatColumn
			(Minus
				(Number 1)
				(Times
					(strength-of (Variable "$X"))
					(strength-of (Variable "$Y"))))
			(Times
				(confidence-of (Variable "$X"))
				(confidence-of (Variable "$Y"))))))

(define exolnk
	(ExecutionOutput
		(DefinedProcedure "has a reddish color")
		(List (Concept "A") (Concept "B"))))

; Was FormulaTruthValue
(define tv-stream (FormulaStream exolnk))

; ----------
; for DynamicUTest::test_dynamic_formula()

(define a-implies-b (Implication (Concept "A") (Concept "B")))

(cog-execute!
	(SetValue a-implies-b tvkey
		(CollectionOfLink (Type 'FormulaStream) (OrderedLink
			(ExecutionOutput
				(DefinedProcedure "has a reddish color")
				(List (Concept "A") (Concept "B")))))))

; -----------
; For DynamicUTest::test_defined_dynamic()
(define p-implies-q (Implication (Concept "P") (Concept "Q")))

(cog-execute!
	(SetValue
		(Implication (Concept "P") (Concept "Q"))
		tvkey
		(CollectionOfLink (Type 'FormulaStream) (OrderedLink
			(ExecutionOutput
				(DefinedProcedure "has a reddish color")
				(List (Concept "A") (Concept "B")))))))

; -------------------------------------------------------------
; for DynamicUTest::test_formula_stream()
(define foo (Concept "foo"))
(define bar (Concept "bar"))
(define akey (Predicate "some key"))
(define bkey (Predicate "other key"))

(cog-set-value! foo akey (FloatValue 1 2 3 4 5))

(define fstream (FormulaStream (Plus (Number 10) (FloatValueOf foo akey))))
(cog-set-value! bar bkey fstream)

; ------- THE END -------
