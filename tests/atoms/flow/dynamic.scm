;
; dynamic.scm -- test-case version of `flow-formulas.scm` example.
;

(use-modules (opencog) (opencog exec))

; For DynamicUTest::test_predicate_formula()

(define A (Concept "A"))
(define B (Concept "B"))

(define tvkey (Predicate "*-TruthValueKey-*"))
(define (strength-of ATOM) (ElementOf (Number 0) (ValueOf ATOM tvkey)))
(define (confidence-of ATOM) (ElementOf (Number 1) (ValueOf ATOM tvkey)))

(cog-set-value! (Concept "A") tvkey (SimpleTruthValue 1 0))
(cog-set-value! (Concept "B") tvkey (SimpleTruthValue 1 0))

; Was FormulaTruthValue
(define fututv
	(FormulaStream
		(FormulaPredicate
			(Minus
				(Number 1)
				(Times (strength-of A) (strength-of B)))
			(Times (confidence-of A) (confidence-of B)))))

; ----------
; For DynamicUTest::test_formula_define()
(DefineLink
   (DefinedPredicate "has a reddish color")
   (FormulaPredicate
      (Minus
         (Number 1)
         (Times
            (strength-of (Variable "$X"))
            (strength-of (Variable "$Y"))))
      (Times
         (confidence-of (Variable "$X"))
         (confidence-of (Variable "$Y")))))

(define evlnk
	(Evaluation
		(DefinedPredicate "has a reddish color")
		(List (Concept "A") (Concept "B"))))

; Was FormulaTruthValue
(define tv-stream (FormulaStream evlnk))

; ----------
; for DynamicUTest::test_dynamic_formula()

(define a-implies-b (Implication (Concept "A") (Concept "B")))

(cog-execute!
	(SetValue
		(Implication (Concept "A") (Concept "B"))
		(Predicate "*-TruthValueKey-*")
		(PromiseLink
			(FormulaPredicate
				(Minus
					(Number 1)
					(Times
						(strength-of (Concept "A"))
						(strength-of (Concept "B"))))
				(Times
					(confidence-of (Concept "A"))
					(confidence-of (Concept "B")))))))

(DefineLink
   (DefinedPredicate "dynamic example")
   (FormulaPredicate
      (Minus
         (Number 1)
         (Times
            (strength-of (Variable "$X"))
            (strength-of (Variable "$Y"))))
      (Times
         (confidence-of (Variable "$X"))
         (confidence-of (Variable "$Y")))))

; -----------
; For DynamicUTest::test_defined_dynamic()
(define p-implies-q (Implication (Concept "P") (Concept "Q")))
;;; XXX FIXME this is currently broken; the old SetTVLink did this
;;; but the new SetValue doesn't (shouldn't) ... we need an ApplyLink...
;;;(cog-execute!
;;;	(SetValue
;;;		(Implication (Concept "P") (Concept "Q"))
;;;		(Predicate "*-TruthValueKey-*")
;;;		(DefinedPredicate "dynamic example")
;;;		(Concept "A") (Concept "B")))

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
