;
; value-flows.scm -- Flowing Values unit test.
; Copy of /examples/atomspace/flows.scm

(use-modules (opencog) (opencog exec))

(define foo (Concept "foo"))
(define bar (Concept "bar"))
(define key (Predicate "some key"))
(define kee (Predicate "other key"))

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
   (DefinedSchema "triangle numbers")
	(Lambda
		(Variable "$X")
		(Divide
			(Times (Variable "$X") (Plus (Variable "$X") (Number 1)))
			(Number 2))))

(define triangle
	(SetValue bar kee
		(DefinedSchema "triangle numbers")
		(FloatValueOf foo key)))
;
; -------- THE END -----------
