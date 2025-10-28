;
; value-flows.scm -- Flowing Values unit test.
; Copy of /examples/atomspace/flows.scm

(use-modules (opencog) (opencog exec))

(define foo (Concept "foo"))
(define bar (Concept "bar"))
(define baz (Concept "baz"))
(define key (Predicate "some key"))
(define kee (Predicate "other key"))
(define kay (Predicate "third key"))

; Expected results
(define seq (FloatValue 1 2 3 4 5))
(define squ (FloatValue 1 4 9 16 25))
(define tri (FloatValue 1 3 6 10 15))

(cog-set-value! foo key seq)

; Copy from foo to bar
(define set-value (SetValue bar kee (FloatValueOf foo key)))

; Try out some math
(define square (SetValue bar kee
	(Times (FloatValueOf foo key) (FloatValueOf foo key))))

(DefineLink
   (DefinedProcedure "triangle numbers")
	(Lambda
		(Variable "$X")
		(Divide
			(Times (Variable "$X") (Plus (Variable "$X") (Number 1)))
			(Number 2))))

(define triangle
	(SetValue bar kee
		(DefinedProcedure "triangle numbers")
		(FloatValueOf foo key)))

; ============================================================
; Test SetValueOn - returns Atom instead of Value
; ============================================================

; SetValueOn copy test - should return the Atom (baz), not the Value
(define set-value-on (SetValueOn baz kee (FloatValueOf foo key)))

; SetValueOn with math - should return the Atom (baz), not the Value
(define square-on (SetValueOn baz kay
	(Times (FloatValueOf foo key) (FloatValueOf foo key))))

; SetValueOn with formula - should return the Atom (baz), not the Value
(define triangle-on
	(SetValueOn baz kay
		(DefinedProcedure "triangle numbers")
		(FloatValueOf foo key)))
;
; -------- THE END -----------
